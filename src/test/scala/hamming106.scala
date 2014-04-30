import DNABarcodes._
import org.scalatest._
import TestHelpers._

class Hamming106Test extends FlatSpec with Matchers {
	
/*	val b = new Hamming106()
	"verifyBarcode" should "check and correct a single error in every position" in {
		val trueBarcode = b.generateBarcode("ACTAGC")
		
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

	val allCodes = permutationsWithRepetitions(List("A","T","C","G"),6)
	
	"All possible Hamming(10,6) codes" should "be equal to 4096" in {
		allCodes.size should be (4096)
	}

	"Hamming(10,6) codes" should "correct errors on all the possibile combinations of barcodes" in {
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
