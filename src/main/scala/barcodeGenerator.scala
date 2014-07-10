package org.ptp
package DNABarcodes {
	/** Trait for barcodeGenerator classes */ 
	trait barcodeGenerator {
		/** An array with positions of parity bits within the Hamming code */
		def parityPositions : Array[Int]
		/** Function to check a Hamming barcode */
		def verifyBarcode(barcode: String) : String
		/** Function to generate the Hamming barcode starting from the DNA seed */
		def generateBarcode(dna: String) : String
	}

}
