package org.ptp
package DNABarcodes


/** Class for Hamming Barcode that has been checked and verified for errors
 * @constructor Return a verified and error corrected DNA barcode
	 @param barcode A String with the barcode to check and error correct
*/
class VerifiedDNABarcode(barcode: String) {

	/** Verify the Barcode and return "NNNNN" if invalid or contains more than one error */ 	
	val verifiedBarcode = barcode.contains('N') match {
		case true => "NNNNN" 
		case false => checkBarcode(barcode)
	}
	/** return true if valid */
	val valid = verifiedBarcode match {
		case "NNNNN" => false
		case _ => true
	}
	/** return true if error correction has been performed succesfully */
	lazy val errorCorrected = isErrorCorrected(barcode,verifiedBarcode) 
	/** return a string describing the type of Hamming code used by the barcode */
	lazy val codeType = verifiedBarcode.size match {
		case 8 => "Hamming(8,4)"
		case 10 => "Hamming(10,5)"
		case 11 => "Hamming(11,6)"
		case 5 => "Invalid Hamming Code"
	}

	private def checkBarcode(barcode: String) : String = {	
		barcode.size match {
			case 8  => return HammingEight.verifyBarcode(barcode)
			case 10 => return HammingTen.verifyBarcode(barcode)
			case 11 => return HammingEleven.verifyBarcode(barcode)
			case _  => return "NNNNN"
		}
	}

	private def isErrorCorrected(barcode: String, verifiedBarcode: String) : Boolean = {	
		if (!valid) {
			return false
		}	
		else if(barcode == verifiedBarcode) {
			return false
		}
		else {
			return true
		}
	}


}
