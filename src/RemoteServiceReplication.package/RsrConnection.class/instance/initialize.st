initialization
initialize

	super initialize.
	isOpen := false.
	reaper := RsrObjectReaper connection: self.
	transactionSpigot := RsrThreadSafeNumericSpigot naturals.
	objectCache := RsrObjectCache new.
	promises := Dictionary new.
	registry := RsrRegistry new.
	serviceRegistry := RsrServiceRegistry
		rsrId: 1
		rsrConnection: self.
	registry retain: serviceRegistry