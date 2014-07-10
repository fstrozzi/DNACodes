package org.ptp
package DNABarcodes
	/** Hamming(8,4) double parity error correcting barcode */
	object HammingEight extends Generics with barcodeGenerator {
	
		val parityPositions = Array(1,2,4)
		val codeLength = 4
		/** generate a new Hamming barcode with the starting DNA seed
		 * @param dna The string with the DNA seed
		*/
		def generateBarcode(dna: String) : String = {
			val data = dna.toCharArray.map(codex(_))
			val p1 = generateParityBit(codeLength, Array(data(0),data(1),data(3)))
			val p2 = generateParityBit(codeLength, Array(data(0),data(2),data(3)))
			val p3 = generateParityBit(codeLength, Array(data(1),data(2),data(3)))
			val p4 = generateParityBit(codeLength, Array(p1,p2,data(0),p3,data(1),data(2),data(3)))
			val hamming = Array(p1,p2,data(0),p3,data(1),data(2),data(3),p4)
			quad2dna(hamming).mkString
		}
		/** Check a barcode and error correct it
		 * @param dna A string with the barcode to check
		*/
		def verifyBarcode(dna: String) : String = {
			this.correctBarcode(codeLength,dna,getParity,parityPositions)
		}

		private def getParity(codeLength: Int, quadCode: Array[Int]) : Array[Int] = {
			val p1 = calculateParityBit(codeLength,Array(quadCode(0),quadCode(2),quadCode(4),quadCode(6)))
			val p2 = calculateParityBit(codeLength,Array(quadCode(1),quadCode(2),quadCode(5),quadCode(6)))
			val p3 = calculateParityBit(codeLength,Array(quadCode(3),quadCode(4),quadCode(5),quadCode(6)))
			val p4 = calculateParityBit(codeLength,quadCode)
			return Array(p1,p2,p3,p4)	
		} 
		


	}
