private-finalization
markForFinalization

	self
		toFinalizeSend: #reap:
		to: self rsrConnection reaper
		with: self rsrId