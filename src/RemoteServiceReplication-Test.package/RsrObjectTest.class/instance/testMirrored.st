running
testMirrored

	| anObject id connection |
	anObject := RsrObject new.
	id := self newId.
	connection := self newTestConnection.
	anObject
		rsrId: id;
		rsrConnection: connection.
	self
		assertObject: anObject
		isMirrored: true
		isDirty: true
		id: id
		connection: connection