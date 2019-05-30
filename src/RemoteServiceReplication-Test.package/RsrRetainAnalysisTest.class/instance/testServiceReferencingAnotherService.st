running
testServiceReferencingAnotherService
	"While this code is structurally similar to #testClientNoInstVars, it ensures
	that Data Objects are actually encoded in-line."

	| referencedService client registry analysis expected |
	referencedService := RsrClientNoInstVars new.
	client := RsrClientReferenceService service: referencedService.
	registry := RsrMockRegistry new.
	analysis := RsrRetainAnalysis
		roots: (Array with: client)
		connection: RsrMockConnection new.
	analysis perform.
	expected := OrderedCollection
		with: (RsrRetainObject object: referencedService encoding: ByteArray new)
		with: (RsrRetainObject object: client encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self
		assert: client isMirrored;
		assert: referencedService isMirrored