DNACodes
========

A quaternary double parity error correcting Hamming DNA Barcodes for NGS application, written in Scala.

This work was adapted from the paper [Generalized DNA Barcode Design Based on Hamming Codes](http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0036852)

The API is minimal and the documentation is available [here](http://fstrozzi.github.io/DNACodes)

```scala
import org.ptp.DNABarcodes._
val barcode = HammingEight.generateBarcode("AATA")
barcode.gc // %GC content

// verify and correct a barcode

val wrongBarcode = "ATCAGCAA"
val correctedBarcode = HammingEight.verifyBarcode(wrongBarcode) // it wil be set to "NNNNN" if barcode can't be corrected

```


For more examples on how to use this library, please have a look at the [tests](https://github.com/fstrozzi/DNACodes/tree/master/src/test/scala)

Copyright(c) 2014 Francesco Strozzi


