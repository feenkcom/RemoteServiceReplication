private-writing
writeCharacter: aCharacter
onto: aStream

	self
		writeControlWord: self immediateOID
		onto: aStream.
	self
		writeControlWord: self characterType
		onto: aStream.
	self
		writeControlWord: aCharacter codePoint
		onto: aStream