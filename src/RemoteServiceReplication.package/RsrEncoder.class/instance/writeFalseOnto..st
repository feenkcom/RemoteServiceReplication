private-writing
writeFalseOnto: aStream

	self
		writeControlWord: self immediateOID
		onto: aStream.
	self
		writeControlWord: self falseType
		onto: aStream