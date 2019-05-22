private-writing
writeCharacter: aCharacter
onto: aStream

	self
		writeInteger: self immediateOID
		onto: aStream.
	self
		writeInteger: self characterType
		onto: aStream.
	self
		writeInteger: aCharacter codePoint
		onto: aStream