running
testMirrored

	| anObject id remoteInterface |
	anObject := RsrObject new.
	id := self newId.
	remoteInterface := self newTestRemoteInterface.
	anObject
		rsrId: id;
		rsrRemoteInterface: remoteInterface.
	self
		assertObject: anObject
		isMirrored: true
		isDirty: true
		id: id
		remoteInterface: remoteInterface