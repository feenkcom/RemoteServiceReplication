asserting
assertControlInteger: expected
encoding: bytes

	| actual |
	actual := self decoder decodeControlWord: bytes readStream.
	self
		assert: actual
		equals: expected