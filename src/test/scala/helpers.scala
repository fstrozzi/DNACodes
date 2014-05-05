
object TestHelpers {

	def permutationsWithRepetitions[T](input: List[T], n: Int) : List[List[T]] = {
  	require(input.length > 0 && n > 0)
    n match {
    	case 1 => for (el <- input) yield List(el)
    	case _ => for (el <- input; perm <- permutationsWithRepetitions(input, n - 1)) yield el :: perm
    }
  }
	
	def randomMutation(barcode: String, numberOfMutations: Int) : String = {
		val r = scala.util.Random
		val alphabet = List('A','C','G','T')
		val barcodeChar = barcode.toCharArray
		val positions = getRandomPositions(numberOfMutations,barcodeChar.size)
		for (position <- positions) {
			val possibleBases = alphabet.filter {_ != barcodeChar(position)}
			val newBase = possibleBases(r.nextInt(3)) 
			barcodeChar(position) = newBase
		}
		return barcodeChar.mkString
	}

	private def getRandomPositions(number: Int, delim: Int) : List[Int] = {
		val r = scala.util.Random
		val positions = List.fill(number){r.nextInt(delim)}
		if (positions.distinct.size == positions.size) {
			return positions
		}
		else {
			getRandomPositions(number,delim)		
		}

	}
 
}
