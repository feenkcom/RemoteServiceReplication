running
testMirrored

	| service id remoteInterface |
	service := RsrClientTestService new.
	id := self newId.
	remoteInterface := self newTestRemoteInterface.
	service
		rsrId: id;
		rsrRemoteInterface: remoteInterface.
	self
		assertObject: service
		isMirrored: true
		isDirty: true
		id: id
		remoteInterface: remoteInterface.
	self
		assert: service remoteSelf class
		equals: RsrTestForwarder