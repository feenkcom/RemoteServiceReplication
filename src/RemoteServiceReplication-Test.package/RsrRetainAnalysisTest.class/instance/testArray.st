running
testArray

	| service array registry analysis expected |
	service := RsrClientNoInstVars new.
	array := (RsrArray new: 1)
		at: 1 put: service;
		yourself.
	registry := RsrMockRegistry new.
	analysis := RsrRetainAnalysis
		roots: (Array with: array)
		connection: RsrMockConnection new.
	analysis perform.
	expected := OrderedCollection
		with: (RsrRetainObject object: service encoding: ByteArray new)
		with: (RsrRetainObject object: array encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self
		assert: service isMirrored;
		assert: array isMirrored