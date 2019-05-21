private-encoding
writeImmediate: anImmediate
onto: aStream

	anImmediate isSymbol
		ifTrue:
			[^self
				writeSymbol: anImmediate
				onto: aStream].
	self error: 'Unsupported Immediate'