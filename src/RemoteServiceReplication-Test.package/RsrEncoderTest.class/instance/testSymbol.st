running-immediates
testSymbol

	| actual |
	actual := ByteArray
		streamContents:
			[:stream |
			self encoder
				encodeSymbol: self genericSymbol
				onto: stream].
	self
		assert: actual
		equals: self genericSymbolEncoding