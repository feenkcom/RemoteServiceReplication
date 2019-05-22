running
testServiceNoInstVars

	| decodedService |
	decodedService := self decode: self serviceNoInstVarsEncoding.
	self
		assert: decodedService class
		equals: RsrServerNoInstVars.
	self
		assert: decodedService rsrId
		equals: 1