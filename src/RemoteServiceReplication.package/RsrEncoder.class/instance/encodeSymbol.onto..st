private-writing
encodeSymbol: aSymbol
onto: aStream

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self symbolType
		onto: aStream.
	self
		encodeStringBody: aSymbol
		onto: aStream