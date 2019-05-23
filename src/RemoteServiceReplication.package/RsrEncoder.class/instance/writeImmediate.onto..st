private-writing
writeImmediate: anImmediate
onto: aStream

	anImmediate isSymbol
		ifTrue:
			[^self
				writeSymbol: anImmediate
				onto: aStream].
	anImmediate isString
		ifTrue:
			[^self
				writeString: anImmediate
				onto: aStream].
	anImmediate isInteger
		ifTrue:
			[^self
				writeImmediateInteger: anImmediate
				onto: aStream].
	anImmediate isCharacter
		ifTrue:
			[^self
				writeCharacter: anImmediate
				onto: aStream].
	anImmediate == nil
		ifTrue:
			[^self writeNilOnto: aStream].
	anImmediate == true
		ifTrue:
			[^self writeTrueOnto: aStream].
	anImmediate == false
		ifTrue:
			[^self writeFalseOnto: aStream].
	self error: 'Unsupported Immediate'