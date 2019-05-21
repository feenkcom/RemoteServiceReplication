decoding
decode: anObjectBytes

	| aDatagram |
	aDatagram := (anObjectBytes size asByteArrayOfSize: self decoder sizeOfInteger), anObjectBytes.
	^(self decoder decode: aDatagram readStream) object