running
testCloseWithRegisteredService
	"It is possible that the connection could disconnect between when an object
	is received and when the upcoming SendMessage or DeliverResponse message is received.
	If this is the case, we coul leak memory due to the caching used to ensure
	the object is stored in memory long enough to process the upcoming message.
	Test to ensure the object is freed on the connection close."

	| service |
	self maximumReclamation.
	self assert: RsrServerNoInstVars allInstances isEmpty.
	service := RsrClientNoInstVars new.
	connectionA serviceRegistry
		at: 'TestValue'
		put: service.
	connectionA close.
	connectionB close.
	service := connectionA := connectionB := nil.
	self maximumReclamation.
	self assert: RsrServerNoInstVars allInstances isEmpty