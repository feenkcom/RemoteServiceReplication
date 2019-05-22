decoding
decodeImmediateObject: aStream

	| type |
	type := self nextInteger: aStream.
	type = 1
		ifTrue: [^self decodeSymbol: aStream].
	self error: 'Invalid immediate type specified'