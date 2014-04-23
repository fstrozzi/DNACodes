package DNABarcodes {

	class Generics {
		
		protected val codex = Map[Char,Int]('A'-> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)
		protected val codexReverse = Map[Int,Char](0 -> 'A', 1 -> 'C', 2 -> 'G', 3 -> 'T')

		protected def generateParityBit(codeLength: Int, codeBits: Array[Int]) : Int = {
			val bit = ((codeLength - codeBits.sum) % 4) % 4
			if(bit < 0) {
				return bit + codeLength
			}
			else {
				return bit
			}
		}

		protected def calculateParityBit(codeBits: Array[Int]) : Int = {
			codeBits.sum % 4 
		}

		protected def quad2dna(hamming: Array[Int]) : Array[Char] = {
			hamming.map(codexReverse(_))
		}

		protected def correctBase(errType: Int, baseAtError: Int) : Char  = {
			val bit = (baseAtError - errType) % 4
			if (bit < 0) {
				return codexReverse(bit + 4)
			}
			else {
				return codexReverse(bit)
			}
		}
	}
}
