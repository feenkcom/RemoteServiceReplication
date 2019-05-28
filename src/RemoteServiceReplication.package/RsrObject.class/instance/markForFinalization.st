private-finalization
markForFinalization

	self
		toFinalizeSend: #releaseOid:
		to: self rsrConnection
		with: self rsrId