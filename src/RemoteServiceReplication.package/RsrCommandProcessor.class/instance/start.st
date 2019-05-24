commands
start

	isRunning := true.
	process := [self runLoop]
		forkAt: self priority
		named: self class name asString, ' loop'