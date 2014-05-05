import DNABarcodes._
import org.scalatest._
import TestHelpers._

class Hamming106Test extends FlatSpec with Matchers {
	
	val b = new Hamming106()
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

	"Hamming(11,6) codes" should "correct errors on all the possibile combinations of barcodes" in {
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
		
		val trueBarcode = b.generateBarcode("ATTCGA")
									 //  GAACTTCGGAG	
		val wrong1 = 			"GTTCTTCGGAG" 
		val wrong2 = 			"ACACTTCGGAG"
		val wrong3 = 			"TAACTTCGGAC"
		val wrong4 = 			"GACCTTAGGAG"
		val wrongTriple = "GTTCTTCGGAA" // triple error, just to check


		b.verifyBarcode(wrong1) should be ("XXXXX")
		b.verifyBarcode(wrong2) should be ("XXXXX")
		b.verifyBarcode(wrong3) should be ("XXXXX")
		b.verifyBarcode(wrong4) should be ("XXXXX")
		b.verifyBarcode(wrongTriple) should be ("XXXXX")
	}
}
