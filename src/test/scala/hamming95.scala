import DNABarcodes._
import org.scalatest._
import TestHelpers._

class Hamming95Test extends FlatSpec with Matchers {
/*	
	val b = new Hamming95()
	
	"verifyBarcode" should "check and correct a single error in every position" in {
		val trueBarcode =  "TAAAAAATC"
		checkTheBarcode("TAAAAAATC")
		
		def checkTheBarcode(barcode: String) {
			val alphabet = Array('A','T','C','G') 
			for (i <- Range(0,barcode.size)) {
				for (base <- alphabet) {
					val wrongBarcode = barcode.toCharArray
					wrongBarcode(i) = base
					val correctedBarcode = b.verifyBarcode(wrongBarcode.mkString)
					correctedBarcode should be (trueBarcode)
				}
			}
		}



	}


	val allCodes = permutationsWithRepetitions(List("A","T","C","G"),5)
	
	"All possible Hamming(9,5) codes" should "be equal to 1024" in {
		allCodes.size should be (1024)
	}

	"Hamming(9,5) codes" should "correct errors for all the possibile combinations of barcodes" in {
		for (code <- allCodes) {
			val trueBarcode = b.generateBarcode(code.mkString)
			checkTheBarcode(trueBarcode)
			
			def checkTheBarcode(barcode: String) {
				val alphabet = Array('A','T','C','G')
				for (i <- Range(0,barcode.size)) {
					for (base <- alphabet) {
						val wrongBarcode = barcode.toCharArray
						wrongBarcode(i) = base
						val correctedBarcode = b.verifyBarcode(wrongBarcode.mkString)
						correctedBarcode should be (trueBarcode)
					}
				}
			}
		}

	}
	*/
}
