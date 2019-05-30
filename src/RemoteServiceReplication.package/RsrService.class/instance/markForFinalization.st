private-lifecycle
markForFinalization

	self isClient
		ifTrue:
			[self
				toFinalizeSend: #reap:
				to: self rsrConnection reaper
				with: self rsrId]