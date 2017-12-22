initialization
initialize
	super initialize.
	isRunningLock  := Mutex new.
	self isRunning: false.
	debug := true.
	