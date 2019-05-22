private-writing
writeSymbol: aSymbol
onto: aStream
	"write OID = 0"
	"write Symbol type"
	"write length of utf-8 encoded string"
	"write utf-8 data"
	
	| utf8Bytes |
	utf8Bytes := aSymbol utf8Encoded.
	self
		writeInteger: self immediateOID
		onto: aStream.
	self
		writeInteger: self symbolType
		onto: aStream.
	self
		writeInteger: utf8Bytes size
		onto: aStream.
	aStream nextPutAll: utf8Bytes