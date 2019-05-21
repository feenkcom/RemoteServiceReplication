asserting
assertReference: bytes
decodesTo: expected

	| actual |
	actual := self decoder decodeObjectReference: bytes readStream.
	self
		assert: actual
		equals: expected