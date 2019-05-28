tests
testRemember

	| id object registry marker |
	id := 1.
	marker := Object new.
	object := RsrObject
		rsrId: id
		rsrConnection: RsrMockConnection new.
	registry := RsrRegistry oidSpigot: RsrNumericSpigot naturals.
	registry remember: object.
	self maximumReclamation.
	self
		assert: (registry at: id ifAbsent: [marker])
		identicalTo: object.
	object := nil.
	self maximumReclamation.
	self
		assert: (registry at: id ifAbsent: [marker])
		identicalTo: marker