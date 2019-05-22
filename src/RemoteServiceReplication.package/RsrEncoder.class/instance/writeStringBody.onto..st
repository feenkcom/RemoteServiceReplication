private-writing
writeStringBody: aString
onto: aStream

	| bytes |
	bytes := aString utf8Encoded.
	self
		writeInteger: bytes size
		onto: aStream.
	aStream nextPutAll: bytes