commands
stop

	isRunning ifFalse: [^self].
	isRunning := false.
	self connection close.
	self stream close.
	process terminate.
	process := nil