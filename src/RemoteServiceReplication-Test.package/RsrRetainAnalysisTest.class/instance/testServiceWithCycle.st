running
testServiceWithCycle
	"Cycles are disallowed for our POC. Perhaps they will get added later?"

	| rootClient referencedClient registry analysis |
	rootClient := RsrClientReferenceService new.
	referencedClient := RsrClientReferenceService service: rootClient.
	rootClient service: referencedClient.
	registry := RsrMockRegistry new.
	analysis := RsrRetainAnalysis
		roots: (Array with: rootClient)
		connection: RsrMockConnection new.
	self
		should: [analysis perform]
		raise: RsrCycleDetected