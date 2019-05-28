running
testRegistration

	| name service marker actual |
	name := 'TestService'.
	service := RsrClientNoInstVars new.
	marker := Object new.
	connectionA serviceRegistry
		at: name
		put: service.
	actual := connectionA serviceRegistry at: name.
	self
		assert: actual
		identicalTo: actual.
	actual := connectionB serviceRegistry at: name.
	self
		assert: actual class
		equals: RsrServerNoInstVars.
	self
		assert: actual rsrId
		equals: service rsrId.
	connectionB serviceRegistry removeKey: name.
	self
		assert: (connectionA serviceRegistry at: name ifAbsent:[marker])
		identicalTo: marker.
	self
		assert: (connectionB serviceRegistry at: name ifAbsent:[marker])
		identicalTo: marker