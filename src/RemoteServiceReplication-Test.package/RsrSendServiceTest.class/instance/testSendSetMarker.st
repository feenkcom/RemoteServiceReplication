running
testSendSetMarker

	| service remoteService |
	service := RsrClientNoInstVars new.
	connectionA registry retain: service.
	service rsrConnection: connectionA.
	remoteService := RsrServerNoInstVars
		rsrId: service rsrId
		rsrConnection: connectionB.
	connectionB registry remember: remoteService.
	service sendSetMarker.
	self assert: remoteService marker