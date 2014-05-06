import org.ptp.DNABarcodes._
import org.scalatest._
import TestHelpers._

class DNABarcodesTest extends FlatSpec with Matchers {

	val allCodes = permutationsWithRepetitions(List("A","T","C","G"),6)
	
	val r = scala.util.Random
	val barcodesList = for(code <- allCodes) yield new DNABarcode(code.mkString).barcode
	val allBarcodes = barcodesList.to[Array]	

	"VerifyBarcode on 3 Million barcodes with random mutations" should "not take too much time" in {
		for	(i <- Range(0,1000001)) {
			val v = new VerifiedDNABarcode(allBarcodes(r.nextInt(allBarcodes.size)))
		}
		for (i <- Range(0,1000001)) {
			val wrongBarcode = randomMutation(allBarcodes(r.nextInt(allBarcodes.size)),1)
			val v = new VerifiedDNABarcode(wrongBarcode)
		}
		for (i <- Range(0,1000001)) {
			val wrongBarcode = randomMutation(allBarcodes(r.nextInt(allBarcodes.size)),2)
			val v = new VerifiedDNABarcode(wrongBarcode)
		}
	}	

}
