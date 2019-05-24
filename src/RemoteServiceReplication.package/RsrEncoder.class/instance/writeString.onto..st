private-writing
writeString: aString
onto: aStream

	self
		writeControlWord: self immediateOID
		onto: aStream.
	self
		writeControlWord: self stringType
		onto: aStream.
	self
		writeStringBody: aString
		onto: aStream