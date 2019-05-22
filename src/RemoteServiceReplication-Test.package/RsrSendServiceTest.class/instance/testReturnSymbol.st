running
testReturnSymbol

	| service remoteService |
	service := RsrClientNoInstVars new.
	connectionA registry addLocal: service.
	service rsrRemoteInterface: connectionA remoteInterface.
	remoteService := RsrServerNoInstVars
		rsrId: service rsrId
		rsrRemoteInterface: connectionB remoteInterface.
	connectionB registry addRemote: remoteService.
	self
		assert: service sendReturnTestSymbol
		equals: #testSymbol