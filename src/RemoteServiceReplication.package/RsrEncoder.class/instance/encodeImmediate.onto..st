private-encoding
encodeImmediate: anImmediate
onto: aStream

	anImmediate isSymbol
		ifTrue:
			[^self
				encodeSymbol: anImmediate
				onto: aStream].
	anImmediate isString
		ifTrue:
			[^self
				encodeString: anImmediate
				onto: aStream].
	anImmediate isInteger
		ifTrue:
			[^self
				encodeImmediateInteger: anImmediate
				onto: aStream].
	anImmediate isCharacter
		ifTrue:
			[^self
				encodeCharacter: anImmediate
				onto: aStream].
	anImmediate == nil
		ifTrue:
			[^self encodeNilOnto: aStream].
	anImmediate == true
		ifTrue:
			[^self encodeTrueOnto: aStream].
	anImmediate == false
		ifTrue:
			[^self encodeFalseOnto: aStream].
	self error: 'Unsupported Immediate'