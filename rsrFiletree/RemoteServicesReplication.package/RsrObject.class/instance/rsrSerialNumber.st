identity
rsrSerialNumber
	^rsrSerialNumber 
		ifNil: [ rsrSerialNumber := (DateAndTime now asNanoSeconds) + 
			(RsrRegistry objectOffset) ]
