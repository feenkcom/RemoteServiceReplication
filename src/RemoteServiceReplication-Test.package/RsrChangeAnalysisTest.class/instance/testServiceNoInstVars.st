running
testServiceNoInstVars

	| client registry analysis expected |
	client := RsrClientNoInstVars new.
	registry := RsrMockRegistry new.
	analysis := RsrRetainAnalysis
		roots: (Array with: client)
		registry: registry
		encoder: RsrMockEncoder new.
	analysis perform.
	expected := OrderedCollection
		with: (RsrRetainObject object: client encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self assert: client isMirrored