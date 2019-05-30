actions
ensureRegistered: anRsrObject

	anRsrObject isMirrored
		ifFalse:
			[anRsrObject
				rsrId: self nextOid;
				rsrConnection: self connection.
			anRsrObject registerWith: self registry]