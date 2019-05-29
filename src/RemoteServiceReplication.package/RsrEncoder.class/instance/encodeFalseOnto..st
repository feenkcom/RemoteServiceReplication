private-encoding
encodeFalseOnto: aStream

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self falseType
		onto: aStream