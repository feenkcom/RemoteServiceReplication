running
testServiceAllDataObjects
	"While this code is structurally similar to #testClientNoInstVars, it ensures
	that Data Objects are actually encoded in-line."

	| client registry analysis expected |
	client := RsrClientAllDataObjects new.
	registry := RsrMockRegistry new.
	analysis := RsrChangeAnalysis
		roots: (Array with: client)
		registry: registry
		encoder: RsrMockEncoder new.
	analysis perform.
	expected := OrderedCollection
		with: (RsrRetainObject object: client encoding: ByteArray new).
	self
		assert: analysis encodings
		equals: expected.
	self assert: client isMirrored