running-immediates
testSymbol

	| actual |
	actual := ByteArray
		streamContents:
			[:stream |
			self encoder
				writeSymbol: self genericSymbol
				onto: stream].
	self
		assert: actual
		equals: self genericSymbolEncoding