package DNABarcodes {
	class barcodeEight extends Generics with barcodeGenerator {
	
		var checksum : Boolean = false
		var errorCorrected : String = _

		def generateBarcode(dna: String) : String = {
			val data = dna.toCharArray.map(codex(_))
			val codeLength = 4
			val p1 = generateParityBit(codeLength, Array(data(0),data(1),data(3)))
			val p2 = generateParityBit(codeLength, Array(data(0),data(2),data(3)))
			val p3 = generateParityBit(codeLength, Array(data(1),data(2),data(3)))
			val p4 = generateParityBit(codeLength, Array(p1,p2,data(0),p3,data(1),data(2),data(3)))
			val hamming = Array(p1,p2,data(0),p3,data(1),data(2),data(3),p4)
			quad2dna(hamming).mkString
		}
	
		// TODO REFACTOR TO MAKE IT MORE DRY
		def checkBarcode(barcode: String) : String = {
			val quadCode = barcode.toCharArray.map(codex(_))
			val parity = parity84(quadCode)
			val errType = parity.dropRight(1).max
			val extraParity = parity(3)
			println(parity.mkString("-"))
			/*if (errType > 0 && extraParity > 0) {
				return uncorrectableCode('N').mkString
			}*/
			if(errType == 0 && extraParity > 0) {
				return uncorrectableCode('N').mkString
			}
			else if (errType > 0) {
				val errorPosition = getErrorPosition(parity)
				val trueBase = correctBase(errType,quadCode(errorPosition))
				val correctedBarcode = barcode.toCharArray
				correctedBarcode(errorPosition) = trueBase
				/*val secondParity = parity84(correctedBarcode.map(codex(_)))
				if(secondParity.dropRight(1).max > 0) {
					return uncorrectableCode('N').mkString
				}
				else {	
					this.checksum = true
					this.errorCorrected = "" */
					return correctedBarcode.mkString
				//}
			}
			this.checksum = true
			return barcode.mkString
		}


		private val parityPositions = Array(1,2,4)
		
		private def uncorrectableCode(filler: Char) : Array[Char] = {
			this.checksum = false
			Array.fill(8) {'N'}

		} 


		private def parity84(quadCode: Array[Int]) : Array[Int] = {
			val p1 = calculateParityBit(Array(quadCode(0),quadCode(2),quadCode(4),quadCode(6)))
			val p2 = calculateParityBit(Array(quadCode(1),quadCode(2),quadCode(5),quadCode(6)))
			val p3 = calculateParityBit(Array(quadCode(3),quadCode(4),quadCode(5),quadCode(6)))
			val p4 = calculateParityBit(quadCode)
			return Array(p1,p2,p3,p4)	
		} 
		

		private def getErrorPosition(parity: Array[Int]) : Int = {
			var finalPosition = 0
			parity.dropRight(1).zipWithIndex.foreach { el	=> // (parityBit,position)
				if(el._1 != 0) {
					finalPosition += parityPositions(el._2)
				}
			}
			finalPosition - 1
		}

	}
}
