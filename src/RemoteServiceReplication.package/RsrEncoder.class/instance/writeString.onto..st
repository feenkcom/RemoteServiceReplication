private-writing
writeString: aString
onto: aStream

	self
		writeInteger: self immediateOID
		onto: aStream.
	self
		writeInteger: self stringType
		onto: aStream.
	self
		writeStringBody: aString
		onto: aStream