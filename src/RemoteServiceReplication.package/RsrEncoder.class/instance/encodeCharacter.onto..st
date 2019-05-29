private-writing
encodeCharacter: aCharacter
onto: aStream

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self characterType
		onto: aStream.
	self
		encodeControlWord: aCharacter codePoint
		onto: aStream