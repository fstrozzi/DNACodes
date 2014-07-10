package org.ptp
package DNABarcodes

/** General Barcode class
 *
 * @constructor Create a new Hamming EC barcode using the provided sequence as seed
 * @param dna the dna sequence seed
 */

class DNABarcode(dna: String) {

	/** It returns a Hamming EC barcode or "NNNNN" if the DNA seed is invalid */
	val barcode = dna.contains('N') match {
		case true => "NNNNN" 
		case false => getBarcode(dna)
	}

	/** Return false if barcode is "NNNNN" */
	val valid = barcode match {
		case "NNNNN" => false
		case _ => true
	}
	
	/** Calculate the %GC content of the Hamming EC Barcode */
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

