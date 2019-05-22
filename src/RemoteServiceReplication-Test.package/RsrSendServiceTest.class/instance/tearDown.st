initialization
tearDown

	connectionA close.
	connectionB close.
	socketPair := connectionA := connectionB := nil.
	super tearDown