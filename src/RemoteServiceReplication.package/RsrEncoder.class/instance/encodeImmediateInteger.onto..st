private-encoding
encodeImmediateInteger: anInteger
onto: aStream

	| bytes immediateType |
	immediateType := anInteger positive
		ifTrue: [self positiveIntegerType]
		ifFalse: [self negativeIntegerType].
	bytes := anInteger asByteArray.
	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: immediateType
		onto: aStream.
	self
		encodeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes