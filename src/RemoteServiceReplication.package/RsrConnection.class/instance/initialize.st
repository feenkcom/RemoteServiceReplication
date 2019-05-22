initialization
initialize

	super initialize.
	transactionSpigot := RsrNumericSpigot naturals.
	retainedObjects := IdentitySet new