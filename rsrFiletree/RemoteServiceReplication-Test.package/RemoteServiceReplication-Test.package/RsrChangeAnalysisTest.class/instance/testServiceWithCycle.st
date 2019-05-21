running
testServiceWithCycle
	"Cycles are disallowed for our POC. Perhaps they will get added later?"

	| rootClient referencedClient registry analysis expected |
	rootClient := RsrClientReferenceService new.
	referencedClient := RsrClientReferenceService service: rootClient.
	rootClient service: referencedClient.
	registry := RsrMockRegistry new.
	analysis := RsrRetainAnalysis
		roots: (Array with: rootClient)
		registry: registry
		encoder: RsrMockEncoder new.
	self
		should: [analysis perform]
		raise: RsrCycleDetected.
	"expected := OrderedCollection
		with: (RsrEncodedObject object: referencedClient encoding: ByteArray new)
		with: (RsrEncodedObject object: rootClient encoding: ByteArray new).
	self
		assert: analysis encodings
		equals: expected.
	self
		assert: rootClient isMirrored;
		assert: referencedClient isMirrored"