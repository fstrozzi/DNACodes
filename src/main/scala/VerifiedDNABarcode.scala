package org.ptp
package DNABarcodes

class VerifiedDNABarcode(barcode: String) {

	
	val verifiedBarcode = barcode.contains('N') match {
		case true => "NNNNN" 
		case false => checkBarcode(barcode)
	}
	
	val valid = verifiedBarcode match {
		case "NNNNN" => false
		case _ => true
	}

	lazy val errorCorrected = isErrorCorrected(barcode,verifiedBarcode) 

	lazy val codeType = verifiedBarcode.size match {
		case 8 => "Hamming(8,4)"
		case 10 => "Hamming(10,5)"
		case 11 => "Hamming(11,6)"
		case 5 => "Not valid Hamming Code"
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
