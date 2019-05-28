initialization
setUp

	| socketPair |
	super setUp.
	socketPair := RsrSocketPair new.
	connectionA := RsrConnection
		socket: socketPair firstSocket
		transactionSpigot: RsrNumericSpigot naturals
		oidSpigot: (RsrNumericSpigot start: 2 step: 1).
	connectionB := RsrConnection
		socket: socketPair secondSocket
		transactionSpigot: RsrNumericSpigot naturals negated
		oidSpigot: RsrNumericSpigot naturals negated.
	connectionA start.
	connectionB start.