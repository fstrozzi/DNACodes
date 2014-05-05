import org.ptp.DNABarcodes._
import org.scalatest._
import TestHelpers._

class HammingElevenTest extends FlatSpec with Matchers {
	
	val b = new HammingEleven()
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
	
	"All possible Hamming(11,6) codes" should "be equal to 4096" in {
		allCodes.size should be (4096)
	}

	"Hamming(11,6) codes" should "correct single errors on all the possibile combinations of barcodes" in {
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

	"Hamming(11,6)" should "detect Double Errors" in {
		for (code <- allCodes) {
			val trueBarcode = b.generateBarcode(code.mkString)
			for (i <- Range(0,1000)) {
				val wrongBarcode = randomMutation(trueBarcode,2)
				b.verifyBarcode(wrongBarcode) should be ("XXXXX")
			}
		}
	}
}
