running
testMirrored

	| service id connection |
	service := RsrClientTestService new.
	id := self newId.
	connection := self newTestConnection.
	service
		rsrId: id;
		rsrConnection: connection.
	self
		assertObject: service
		isMirrored: true
		id: id
		connection: connection.
	self
		assert: service remoteSelf class
		equals: RsrMockForwarder