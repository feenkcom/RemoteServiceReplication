running
testInitialization

	| service |
	service := RsrClientTestService new.
	self
		assertObject: service
		isMirrored: false
		id: nil
		connection: nil.