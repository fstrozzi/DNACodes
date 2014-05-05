import org.ptp.DNABarcodes._
import org.scalatest._
import TestHelpers._

class HammingEightTest extends FlatSpec with Matchers {

	val b = new HammingEight()
	"verifyBarcode" should "check and correct a single error in every position" in {
		val trueBarcode = b.generateBarcode("AATA")
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

 val allCodes = permutationsWithRepetitions(List("A","T","C","G"),4)
 "All possible Hamming(8,4) codes" should "be equal to 256" in {
		allCodes.size should be (256)
	}

	"Hamming(8,4)" should "detect Double Errors" in {
		for (code <- allCodes) {
			val trueBarcode = b.generateBarcode(code.mkString)
			for (i <- Range(0,1000)) {
				val wrongBarcode = randomMutation(trueBarcode,2)
				b.verifyBarcode(wrongBarcode) should be ("XXXXX")
			}
		}
		
	}	
 
	
	"Hamming(8,4) codes" should "correct single errors on all the possibile combinations of barcodes" in {
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

}
