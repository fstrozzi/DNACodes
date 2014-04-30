package DNABarcodes {

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
		
		def correctBarcode(codeLength: Int, barcode: String, getParity: (Int,Array[Int]) => Array[Int], parityPositions: Array[Int]) : String = {
			val quadCode = barcode.toCharArray.map(codex(_))
			val doubleParity = getParity(codeLength,quadCode)
			println("PARITY:"+doubleParity.mkString("-"))
			val parity = doubleParity.dropRight(1)
			val extraParity = doubleParity.last
			val errType = parity.max
			if (errType > 0 && extraParity == errType) {
				val errorPosition = getErrorPosition(parity,parityPositions)
				val trueBase = correctBase(codeLength,errType,quadCode(errorPosition))
				val correctedBarcode = barcode.toCharArray
				correctedBarcode(errorPosition) = trueBase
				return correctedBarcode.mkString
			}
			else if (errType > 0 && extraParity != errType) {
				return "XXXXX"
			}
			// WE KNOW THE ERROR IS IN THE EXTRA PARITY BIT SO RUN ITERATIVELY TO FIND THE CORRECT BASE
			else if (errType == 0 && extraParity != 0) {
				val correctedBarcode = barcode.toCharArray
				correctedBarcode(correctedBarcode.size-1) = codexReverse(extraParity)
				return correctedBarcode.mkString
			}
			return barcode.mkString
		}

	}
}
