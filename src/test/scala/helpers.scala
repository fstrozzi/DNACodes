
object TestHelpers {
  def permutationsWithRepetitions[T](input: List[T], n: Int) : List[List[T]] = {
  	require(input.length > 0 && n > 0)
    n match {
    	case 1 => for (el <- input) yield List(el)
    	case _ => for (el <- input; perm <- permutationsWithRepetitions(input, n - 1)) yield el :: perm
    }
  }
	
	
 
}
