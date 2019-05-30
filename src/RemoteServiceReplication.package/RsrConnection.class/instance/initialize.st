initialization
initialize

	super initialize.
	isOpen := false.
	reaper := RsrObjectReaper connection: self.
	transactionSpigot := RsrNumericSpigot naturals.
	objectCache := RsrObjectCache new.
	promises := Dictionary new.
	serviceRegistry := RsrServiceRegistry
		rsrId: 1
		rsrConnection: self