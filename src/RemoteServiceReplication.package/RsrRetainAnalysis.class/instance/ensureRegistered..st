actions
ensureRegistered: anRsrObject

	anRsrObject isMirrored
		ifFalse: [registry retain: anRsrObject]