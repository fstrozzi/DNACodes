object DNABarcodes extends App {
	
	val codex = Map[Char,Int]('A'-> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)
	val codexReverse = Map[Int,Char](0 -> 'A', 1 -> 'C', 2 -> 'G', 3 -> 'T')
	val parityPositions = Array(1,2,4)

	def hamming84(dna: Array[Char]) : Array[Char] = {
		val data = dna.map(codex(_))
		val codeLength = 4
		val p1 = generateParityBit(codeLength, Array(data(0),data(1),data(3)))
		val p2 = generateParityBit(codeLength, Array(data(0),data(2),data(3)))
		val p3 = generateParityBit(codeLength, Array(data(1),data(2),data(3)))
		val p4 = generateParityBit(codeLength, data)
		val hamming = Array(p1,p2,data(0),p3,data(1),data(2),data(3),p4)
		quad2dna(hamming)
	}
	
	def generateParityBit(codeLength: Int, codeBits: Array[Int]) : Int = {
		val bit = ((codeLength - codeBits.sum) % 4) % 4
		if(bit < 0) {
			return bit + codeLength
		}
		else {
			return bit
		}
	}

	def checkCode84(barcode: Array[Char]) : Array[Char] = {
		val quadCode = barcode.map(codex(_))
		val parity = parity84(quadCode)
		println("Parity Code: "+parity.mkString("-"))
		val errType = parity.dropRight(1).max
		val extraParity = parity(3)
		if (errType > 0 && extraParity == 0) {
			return uncorrectableCode('N')
		}
		else if (errType > 0) {
			val errorPosition = getErrorPosition(parity)
			println("ErrorPosition:"+errorPosition)
			val trueBase = correctBase(errType,quadCode(errorPosition))
			val correctedBarcode = barcode
			correctedBarcode(errorPosition) = trueBase
			val secondParity = parity84(correctedBarcode.map(codex(_)))
			if(secondParity.dropRight(1).max > 0) {
				return uncorrectableCode('N')
			}
			else {	
				return correctedBarcode
			}
		}
		return barcode
	}

	def uncorrectableCode(filler: Char) : Array[Char] = {
		Array.fill(8) {'N'}
	} 


	def quad2bin(code: Array[Int]) : Array[Int] = {
		code.map(el => {if(el > 0) 1 ; else 0})
	}

	def parity84(quadCode: Array[Int]) : Array[Int] = {
		val p1 = calculateParityBit(Array(quadCode(0),quadCode(2),quadCode(4),quadCode(6)))
		val p2 = calculateParityBit(Array(quadCode(1),quadCode(2),quadCode(5),quadCode(6)))
		val p3 = calculateParityBit(Array(quadCode(3),quadCode(4),quadCode(5),quadCode(6)))
		val p4 = calculateParityBit(quadCode)
		return Array(p1,p2,p3,p4)	
	} 
	

	def calculateParityBit(codeBits: Array[Int]) : Int = {
		codeBits.sum % 4 
	}

	def quad2dna(hamming: Array[Int]) : Array[Char] = {
		hamming.map(codexReverse(_))
	}

	def getErrorPosition(parity: Array[Int]) : Int = {
		var finalPosition = 0
		parity.dropRight(1).zipWithIndex.foreach { el	=> // (parityBit,position)
			if(el._1 != 0) {
				finalPosition += parityPositions(el._2)
			}
		}
		finalPosition - 1
	}

	def correctBase(errType: Int, baseAtError: Int) : Char  = {
		val bit = (baseAtError - errType) % 4
		if (bit < 0) {
			return codexReverse(bit + 4)
		}
		else {
			return codexReverse(bit)
		}
	}

// TEST ZONE 

	val barcode = Array('A','C','T','A')
	val code = hamming84(barcode)
	println("Original Code: " + code.mkString)
	val wrongCode = code
	wrongCode(1) = 'G'
	//wrongCode(6) = 'T'
	println("Wrong Code: "+wrongCode.mkString)
	val corrected = checkCode84(wrongCode)
	println("Corrected Code: "+ corrected.mkString)
	if(code == corrected) {
		println("OK")
	}
	else {
		println("ERROR")
	}
// END TEST ZONE

}
