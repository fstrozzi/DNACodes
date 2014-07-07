package org.ptp
package DNABarcodes {

	trait barcodeGenerator {
		def parityPositions : Array[Int]
		def verifyBarcode(barcode: String) : String
		def generateBarcode(dna: String) : String
	}

}
