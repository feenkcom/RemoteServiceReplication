initialization
tearDown

	connectionA close.
	connectionB close.
	connectionA := connectionB := nil.
	super tearDown