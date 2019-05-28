initialization
initialize

	super initialize.
	transactionSpigot := RsrNumericSpigot naturals.
	objectCache := RsrObjectCache new.
	promises := Dictionary new.
	serviceRegistry := RsrServiceRegistry
		rsrId: 1
		rsrConnection: self