private-writing
encodeTrueOnto: aStream

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self trueType
		onto: aStream