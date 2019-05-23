private-writing
writeNilOnto: aStream

	self
		writeInteger: self immediateOID
		onto: aStream.
	self
		writeInteger: self nilType
		onto: aStream