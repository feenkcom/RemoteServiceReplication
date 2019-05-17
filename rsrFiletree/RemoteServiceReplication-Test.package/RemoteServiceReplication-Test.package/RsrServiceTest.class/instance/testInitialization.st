running
testInitialization

	| service |
	service := RsrClientTestService new.
	self
		assertObject: service
		isMirrored: false
		isDirty: true
		id: nil
		remoteInterface: nil.