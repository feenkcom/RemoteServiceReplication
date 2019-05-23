private-writing
writeTrueOnto: aStream

	self
		writeInteger: self immediateOID
		onto: aStream.
	self
		writeInteger: self trueType
		onto: aStream