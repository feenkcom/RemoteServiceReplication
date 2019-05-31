initialization
setUp

	| socketPair |
	super setUp.
	socketPair := RsrSocketPair new.
	connectionA := RsrConnection
		socket: socketPair firstSocket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals
		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1).
	connectionB := RsrConnection
		socket: socketPair secondSocket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated
		oidSpigot: RsrThreadSafeNumericSpigot naturals negated.
	connectionA start.
	connectionB start.