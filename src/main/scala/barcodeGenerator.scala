package DNABarcodes {


	trait barcodeGenerator {
		def generateBarcode(dna: String) : String
		def checkBarcode(barcode: String) : String
	}

}
