private-writing
writeTrueOnto: aStream

	self
		writeControlWord: self immediateOID
		onto: aStream.
	self
		writeControlWord: self trueType
		onto: aStream