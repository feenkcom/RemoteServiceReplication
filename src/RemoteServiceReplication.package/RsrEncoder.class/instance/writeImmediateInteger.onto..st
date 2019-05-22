private-writing
writeImmediateInteger: anInteger
onto: aStream

	| bytes immediateType |
	immediateType := anInteger positive
		ifTrue: [self positiveIntegerType]
		ifFalse: [self negativeIntegerType].
	bytes := anInteger asByteArray.
	self
		writeInteger: self immediateOID
		onto: aStream.
	self
		writeInteger: immediateType
		onto: aStream.
	self
		writeInteger: bytes size
		onto: aStream.
	aStream nextPutAll: bytes