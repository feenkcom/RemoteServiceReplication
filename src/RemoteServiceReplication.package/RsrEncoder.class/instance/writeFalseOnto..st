private-writing
writeFalseOnto: aStream

	self
		writeInteger: self immediateOID
		onto: aStream.
	self
		writeInteger: self falseType
		onto: aStream