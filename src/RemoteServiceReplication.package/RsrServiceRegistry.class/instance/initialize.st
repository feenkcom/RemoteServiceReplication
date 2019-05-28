private-initialization
initialize

	super initialize.
	services := Dictionary new.
	mutex := Semaphore forMutualExclusion