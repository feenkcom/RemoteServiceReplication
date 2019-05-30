running
testCloseWithDanglingObject
	"It is possible that the connection could disconnect between when an object
	is received and when the upcoming SendMessage or DeliverResponse message is received.
	If this is the case, we coul leak memory due to the caching used to ensure
	the object is stored in memory long enough to process the upcoming message.
	Test to ensure the object is freed on the connection close."

	| service command |
	self maximumReclamation.
	self assert: RsrClientNoInstVars allInstances isEmpty.
	service := RsrServerNoInstVars new.
	service
		rsrId: 2;
		rsrConnection: connectionA.
	command := RsrRetainObject object: service.
	command
		encodeUsing: connectionA encoder;
		writeUsing: connectionA commandWriter.
	connectionA commandWriter flush.
	(Delay forSeconds: 1) wait.
	connectionA close.
	connectionB close.
	service := command := nil.
	self maximumReclamation.
	self assert: RsrClientNoInstVars allInstances isEmpty