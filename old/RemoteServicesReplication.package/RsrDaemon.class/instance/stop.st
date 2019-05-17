accessing
stop
	self isRunning: false.
	connectionSocket closeAndDestroy.