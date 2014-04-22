object DNABarcodes extends App {
	val codex = Map[Char,Int]('A'-> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)
	val codexReverse = Map[Int,Char](0 -> 'A', 1 -> 'C', 2 -> 'G', 3 -> 'T')

	def hamming74(dna: Array[Char]) : Array[Int] = {
		val data = dna.map(codex(_))
		val codeLength = 4
		val p1 = generateParityBit(codeLength, Array(data(0),data(1),data(3)))
		val p2 = generateParityBit(codeLength, Array(data(0),data(2),data(3)))
		val p3 = generateParityBit(codeLength, Array(data(1),data(2),data(3)))
		val hamming = Array(p1,p2,data(0),p3,data(1),data(2),data(3))
		hamming
	}
	
	def generateParityBit(codeLength: Int, codeBits: Array[Int]) : Int = {
		val bit = ((codeLength - codeBits.sum) % codeLength) % codeLength
		if(bit < 0) {
			return bit + codeLength
		}
		else {
			return bit
		}
	}

	def quad2dna(hamming: Array[Int]) : Array[Char] = {
		val dna = hamming.map(codexReverse(_))
		dna
	}

	val barcode = Array('A','T','C','G')
	val code = hamming74(barcode)
	println(quad2dna(code).mkString)


}
