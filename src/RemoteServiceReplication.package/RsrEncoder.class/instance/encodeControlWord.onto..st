private-encoding
encodeControlWord: anInteger
onto: aStream

	| encodedBytes |
	(anInteger between: self controlWordMin and: self controlWordMax)
		ifFalse: [self error: anInteger printString, ' is outside the supported size of a control word.'].
	encodedBytes := (anInteger positive
		ifTrue: [anInteger]
		ifFalse: [(2 raisedTo: 64) + anInteger]) asByteArrayOfSize: self sizeOfInteger.
	aStream nextPutAll: encodedBytes