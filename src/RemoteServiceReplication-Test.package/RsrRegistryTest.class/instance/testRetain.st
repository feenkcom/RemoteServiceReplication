tests
testRetain

	| id object registry marker |
	id := 1.
	marker := Object new.
	object := RsrObject
		rsrId: id
		rsrConnection: RsrMockConnection new.
	registry := RsrRegistry oidSpigot: RsrNumericSpigot naturals.
	registry retain: object.
	object := nil.
	self maximumReclamation.
	object := registry at: id ifAbsent: [marker].
	self
		deny: object
		equals: marker.
	self
		assert: object class
		equals: RsrObject.
	self
		assert: object rsrId
		equals: id