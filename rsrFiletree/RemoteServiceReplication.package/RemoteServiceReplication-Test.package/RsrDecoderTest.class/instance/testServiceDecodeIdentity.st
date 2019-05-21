running
testServiceDecodeIdentity
	"Ensure that decoding an object multiple times results in
	a single object getting created."

	| firstService secondService |
	firstService := self decode: self serviceNoInstVarsEncoding.
	secondService := self decode: self serviceNoInstVarsEncoding.
	self
		assert: firstService
		identicalTo: secondService