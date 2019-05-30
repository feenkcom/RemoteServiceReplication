tests
testRetain

	| id object registry marker |
	id := 1.
	marker := Object new.
	object := RsrServerNoInstVars
		rsrId: id
		rsrConnection: RsrMockConnection new.
	registry := RsrRegistry new.
	registry retain: object.
	object := nil.
	self maximumReclamation.
	object := registry at: id ifAbsent: [marker].
	self
		deny: object
		equals: marker.
	self
		assert: object class
		equals: RsrServerNoInstVars.
	self
		assert: object rsrId
		equals: id