private-writing
writeImmediateInteger: anInteger
onto: aStream

	| bytes immediateType |
	immediateType := anInteger positive
		ifTrue: [self positiveIntegerType]
		ifFalse: [self negativeIntegerType].
	bytes := anInteger asByteArray.
	self
		writeControlWord: self immediateOID
		onto: aStream.
	self
		writeControlWord: immediateType
		onto: aStream.
	self
		writeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes