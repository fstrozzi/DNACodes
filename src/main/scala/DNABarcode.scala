package org.ptp
package DNABarcodes

class DNABarcode(dna: String) {

	val barcode = dna.contains('N') match {
		case true => "NNNNN" 
		case false => getBarcode(dna)
	}
	val valid = barcode match {
		case "NNNNN" => false
		case _ => true
	}

	lazy val gc = ((barcode.toCharArray.foldLeft(0){(z,i) => {if(i == 'C' || i == 'G') z+1; else z+0}}) / barcode.size.toFloat) * 100

	private def getBarcode(dna: String) : String = {
		dna.size match {
			case 4 => return HammingEight.generateBarcode(dna)
			case 5 => return HammingTen.generateBarcode(dna)
			case 6 => return HammingEleven.generateBarcode(dna)
			case _ => return "NNNNN"
		}
	}
}

