private-writing
writeSymbol: aSymbol
onto: aStream
	"write OID = 0"
	"write Symbol type"
	"write length of utf-8 encoded string"
	"write utf-8 data"

	self
		writeControlWord: self immediateOID
		onto: aStream.
	self
		writeControlWord: self symbolType
		onto: aStream.
	self
		writeStringBody: aSymbol
		onto: aStream