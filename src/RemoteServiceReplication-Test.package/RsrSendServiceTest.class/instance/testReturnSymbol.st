running
testReturnSymbol

	| service remoteService |
	service := RsrClientNoInstVars new.
	connectionA registry retain: service.
	service rsrConnection: connectionA.
	remoteService := RsrServerNoInstVars
		rsrId: service rsrId
		rsrConnection: connectionB.
	connectionB registry remember: remoteService.
	self
		assert: service sendReturnTestSymbol
		equals: #testSymbol