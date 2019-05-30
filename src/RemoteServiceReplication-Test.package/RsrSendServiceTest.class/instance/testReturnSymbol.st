running
testReturnSymbol

	| service |
	service := RsrClientNoInstVars new.
	self serviceRegistryA
		at: 'TestService'
		put: service.
	self
		assert: service sendReturnTestSymbol
		equals: #testSymbol