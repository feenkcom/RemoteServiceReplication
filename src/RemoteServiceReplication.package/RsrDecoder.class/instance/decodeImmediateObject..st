decoding
decodeImmediateObject: aStream

	| type |
	type := self decodeControlWord: aStream.
	type = self symbolType
		ifTrue: [^self decodeSymbol: aStream].
	type = self stringType
		ifTrue: [^self decodeString: aStream].
	type = self positiveIntegerType
		ifTrue: [^self decodeImmediateInteger: aStream].
	type = self negativeIntegerType
		ifTrue: [^self decodeNegativeInteger: aStream].
	type = self characterType
		ifTrue: [^self decodeCharacter: aStream].
	type = self nilType
		ifTrue: [^nil].
	type = self trueType
		ifTrue: [^true].
	type = self falseType
		ifTrue: [^false].
	self error: 'Invalid immediate type specified (', type printString, ')'