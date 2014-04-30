package DNABarcodes {
	class Hamming106 extends Generics with barcodeGenerator {
	
		val parityPositions = Array(1,2,4,8)
		val codeLength = 6
		
		def generateBarcode(dna: String) : String = {
			val data = dna.toCharArray.map(codex(_))
			val p1 = generateParityBit(codeLength, Array(data(0),data(1),data(3),data(4)))
			val p2 = generateParityBit(codeLength, Array(data(0),data(2),data(3),data(5)))
			val p3 = generateParityBit(codeLength, Array(data(1),data(2),data(3)))
			val p4 = generateParityBit(codeLength, Array(data(4),data(5)))
			val hamming = Array(p1,p2,data(0),p3,data(1),data(2),data(3),p4,data(4),data(5))
			quad2dna(hamming).mkString
		}

		def verifyBarcode(dna: String) : String = {
			this.correctBarcode(codeLength,dna,getParity,parityPositions)
		}

		private def getParity(codeLength: Int, quadCode: Array[Int]) : Array[Int] = {
			val p1 = calculateParityBit(codeLength, Array(quadCode(0),quadCode(2),quadCode(4),quadCode(6),quadCode(8)))
			val p2 = calculateParityBit(codeLength, Array(quadCode(1),quadCode(2),quadCode(5),quadCode(6),quadCode(9)))
			val p3 = calculateParityBit(codeLength, Array(quadCode(3),quadCode(4),quadCode(5),quadCode(6)))
			val p4 = calculateParityBit(codeLength, Array(quadCode(7),quadCode(8),quadCode(9)))
			return Array(p1,p2,p3,p4)
		} 
		


	}
}
