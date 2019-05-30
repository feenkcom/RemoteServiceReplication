running
testInitialization

	| localObject |
	localObject := RsrObject new.
	self
		assertObject: localObject
		isMirrored: false
		id: nil
		connection: nil