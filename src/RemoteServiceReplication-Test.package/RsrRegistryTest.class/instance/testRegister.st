tests
testRegister

	| id object registry marker |
	id := 1.
	marker := Object new.
	object := RsrClientNoInstVars
		rsrId: id
		rsrConnection: RsrMockConnection new.
	registry := RsrRegistry new.
	registry register: object.
	self maximumReclamation.
	self
		assert: (registry at: id ifAbsent: [marker])
		identicalTo: object.
	object := nil.
	self maximumReclamation.
	self
		assert: (registry at: id ifAbsent: [marker])
		identicalTo: marker