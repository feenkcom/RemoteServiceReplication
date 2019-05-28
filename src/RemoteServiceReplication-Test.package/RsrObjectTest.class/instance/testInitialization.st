running
testInitialization

	| localObject |
	localObject := RsrObject new.
	self
		assertObject: localObject
		isMirrored: false
		isDirty: true
		id: nil
		connection: nil