initialization
initialize

	super initialize.
	retainCommands := OrderedCollection new.
	inFlight := IdentitySet new