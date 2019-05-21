commands
start

	process := [self runLoop]
		forkAt: self readingPriority
		named: 'RsrReader loop'