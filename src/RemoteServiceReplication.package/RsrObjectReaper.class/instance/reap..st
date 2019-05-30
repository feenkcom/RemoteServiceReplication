reaping
reap: anOID

	self connection ifNotNil: [:connection | connection releaseOid: anOID]