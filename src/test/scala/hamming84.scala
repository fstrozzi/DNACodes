import DNABarcodes._
import org.scalatest._
import TestHelpers._

class Hamming84Test extends FlatSpec with Matchers {

	val b = new Hamming84()
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


	"Hamming(8,4)" should "detect Double Errors" in {
		
		val trueBarcode = b.generateBarcode("AAAC")
							//  TTATAACG		
		val wrong1 = "TGATAACA" 
		val wrong2 = "CGATAACG"
		val wrong3 = "TTATAAAA"
		val wrong4 = "TAACAACG"
		val wrongTriple = "ATACAACT" // triple error, just to check


		b.verifyBarcode(wrong1) should be ("XXXXX")
		b.verifyBarcode(wrong2) should be ("XXXXX")
		b.verifyBarcode(wrong3) should be ("XXXXX")
		b.verifyBarcode(wrong4) should be ("XXXXX")
		b.verifyBarcode(wrongTriple) should be ("XXXXX")

	}

	val allCodes = permutationsWithRepetitions(List("A","T","C","G"),4)
	
	"All possible Hamming(8,4) codes" should "be equal to 256" in {
		allCodes.size should be (256)
	}

	"Hamming(8,4) codes" should "correct errors on all the possibile combinations of barcodes" in {
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
