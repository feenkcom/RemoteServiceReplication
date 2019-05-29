private-writing
encodeNilOnto: aStream

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self nilType
		onto: aStream