running
testSendSetMarker

	| service remoteService |
	service := RsrClientNoInstVars new.
	self serviceRegistryA
		at: 'TestService'
		put: service.
	remoteService := self serviceRegistryB at: 'TestService'.
	service sendSetMarker.
	self assert: remoteService marker