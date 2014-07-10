package org.ptp	
package DNABarcodes

	/** Abstract class to include commong functions for DNA Barcodes generation and verification */
	abstract class Generics {
		
		
		protected val codex = Map[Char,Int]('A'-> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)
		protected val codexReverse = Map[Int,Char](0 -> 'A', 1 -> 'C', 2 -> 'G', 3 -> 'T')

		protected def generateParityBit(codeLength: Int, codeBits: Array[Int]) : Int = {
			val bit = ((4 - codeBits.sum) % 4) % 4
			if(bit < 0) {
				return bit + 4
			}
			else {
				return bit
			}
		}

		protected def getErrorPosition(parity: Array[Int], parityPositions: Array[Int]) : Int = {
			var finalPosition = 0
			parity.zipWithIndex.foreach { el => // (parityBit,position)
				if(el._1 != 0) {
					finalPosition += parityPositions(el._2)
				}
			}
			finalPosition - 1
		}
		
		protected def calculateParityBit(codeLength: Int,codeBits: Array[Int]) : Int = {
			return codeBits.sum % 4 
		}

		protected def quad2dna(hamming: Array[Int]) : Array[Char] = {
			hamming.map(codexReverse(_))
		}

		protected def correctBase(codeLength: Int, errType: Int, baseAtError: Int) : Char  = {
			val bit = (baseAtError - errType) % 4
			if (bit < 0) {
				return codexReverse(bit + 4)
			}
			else {
				return codexReverse(bit)
			}
		}
		/** Attempt error correction on a Hamming barcode 
		 * @param codeLength The length of the barcode
			 @param barcode A string with the actual barcode
			 @param getParity A function which calculates the parity of a barcode
			 @param parityPositions An array which stores the positions of the parity bit in the Hamming barcode
		*/
		def correctBarcode(codeLength: Int, barcode: String, getParity: (Int,Array[Int]) => Array[Int], parityPositions: Array[Int]) : String = {
			val quadCode = barcode.toCharArray.map(codex(_))
			val doubleParity = getParity(codeLength,quadCode)
			val parity = doubleParity.dropRight(1)
			val extraParity = doubleParity.last
			val errType = parity.max
			
			if(doubleErrors(parity,extraParity,doubleParity,errType)) {
				return "NNNNN" // there are two or more uncorrectable errors
			}
			else if (errType > 0 && extraParity == errType) { // there is one correctable error
				val errorPosition = getErrorPosition(parity,parityPositions)
				val trueBase = correctBase(codeLength,errType,quadCode(errorPosition))
				val correctedBarcode = barcode.toCharArray
				correctedBarcode(errorPosition) = trueBase
				return correctedBarcode.mkString
			} 
			// single correctable error in the extra parity bit 
			else if (errType == 0 && extraParity != 0) {
				val correctedBarcode = barcode.toCharArray
				val trueBase = (quadCode.last - extraParity )%4
				if(trueBase < 0) {
					correctedBarcode(correctedBarcode.size-1) = codexReverse(trueBase + 4)
				}
				else {
					correctedBarcode(correctedBarcode.size-1) = codexReverse(trueBase)
				}
				return correctedBarcode.mkString
			}
			return barcode.mkString // no errors detected
		}

		private def doubleErrors(parity: Array[Int],extraParity: Int, doubleParity: Array[Int], errType: Int) : Boolean = {
			if (doubleParity.distinct.size > 2) {
				return true
			}
			else if (parity.filter(_ != 0).distinct.size >= 2 && extraParity > 0) {
				return true
			}
			else if (errType > 0 && extraParity != errType) {
				return true
			}
			else {
				return false
			}
		}
	
	}
