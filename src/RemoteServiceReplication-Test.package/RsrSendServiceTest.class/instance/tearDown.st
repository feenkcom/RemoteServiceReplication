initialization
tearDown

	socketPair close.
	socketPair := connectionA := connectionB := nil.
	super tearDown