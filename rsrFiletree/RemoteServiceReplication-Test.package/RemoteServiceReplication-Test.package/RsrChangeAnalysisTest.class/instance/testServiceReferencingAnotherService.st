running
testServiceReferencingAnotherService
	"While this code is structurally similar to #testClientNoInstVars, it ensures
	that Data Objects are actually encoded in-line."

	| referencedService client registry analysis expected |
	referencedService := RsrClientNoInstVars new.
	client := RsrClientReferenceService service: referencedService.
	registry := RsrMockRegistry new.
	analysis := RsrChangeAnalysis
		roots: (Array with: client)
		registry: registry
		encoder: RsrMockEncoder new.
	analysis perform.
	expected := OrderedCollection
		with: (RsrEncodedObject object: referencedService encoding: ByteArray new)
		with: (RsrEncodedObject object: client encoding: ByteArray new).
	self
		assert: analysis encodings
		equals: expected.
	self
		assert: client isMirrored;
		assert: referencedService isMirrored