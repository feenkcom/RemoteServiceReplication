private-writing
encodeStringBody: aString
onto: aStream

	| bytes |
	bytes := aString utf8Encoded.
	self
		encodeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes