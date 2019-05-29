private-encoding
encodeString: aString
onto: aStream

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self stringType
		onto: aStream.
	self
		encodeStringBody: aString
		onto: aStream