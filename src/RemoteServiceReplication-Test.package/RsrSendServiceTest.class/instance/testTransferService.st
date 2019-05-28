running
testTransferService

	| service command remoteService |
	service := RsrClientNoInstVars new.
	connectionA registry retain: service.
	command := RsrRetainObject object: service.
	command
		encodeUsing: connectionA encoder;
		writeUsing: connectionA commandWriter.
	connectionA commandWriter flush.
	(Delay forSeconds: 1) wait.
	remoteService := connectionB registry at: service rsrId.
	self
		deny: remoteService
		identicalTo: service.
	self
		assert: remoteService class
		equals: RsrServerNoInstVars.
	self
		assert: remoteService rsrId
		equals: service rsrId