running-immediates
assertControlInteger: anInteger
encoding: expected

	| actual |
	actual := ByteArray streamContents: [:stream | self encoder writeControlWord: anInteger onto: stream].
	self
		assert: actual
		equals: expected