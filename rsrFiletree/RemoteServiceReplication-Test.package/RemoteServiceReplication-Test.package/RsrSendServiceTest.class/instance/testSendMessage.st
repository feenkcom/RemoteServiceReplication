running
testSendMessage

	| service remoteService |
	service := RsrClientNoInstVars new.
	connectionA registry addLocal: service.
	service rsrRemoteInterface: connectionA remoteInterface.
	remoteService := RsrServerNoInstVars
		rsrId: service rsrId
		rsrRemoteInterface: connectionB remoteInterface.
	connectionB registry addRemote: remoteService.
	service markClean.
	remoteService markClean.
	service sendSetMarker.
	(Delay forSeconds: 1) wait.
	self assert: remoteService marker