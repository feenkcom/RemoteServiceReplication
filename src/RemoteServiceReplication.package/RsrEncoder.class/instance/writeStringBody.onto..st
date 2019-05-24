private-writing
writeStringBody: aString
onto: aStream

	| bytes |
	bytes := aString utf8Encoded.
	self
		writeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes