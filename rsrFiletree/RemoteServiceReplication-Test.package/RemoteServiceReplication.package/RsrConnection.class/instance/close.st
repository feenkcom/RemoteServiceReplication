lifecycle
close

	commandReader stop.
	commandWriter stop.
	socket close.
	commandReader := commandWriter := socket := nil