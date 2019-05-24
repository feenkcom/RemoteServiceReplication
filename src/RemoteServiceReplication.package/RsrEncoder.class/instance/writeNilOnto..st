private-writing
writeNilOnto: aStream

	self
		writeControlWord: self immediateOID
		onto: aStream.
	self
		writeControlWord: self nilType
		onto: aStream