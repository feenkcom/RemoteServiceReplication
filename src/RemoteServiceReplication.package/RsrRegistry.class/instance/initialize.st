initialization
initialize

	super initialize.
	map := WeakValueDictionary new.
	storage := IdentitySet new.
	mutex := Semaphore forMutualExclusion