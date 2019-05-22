commands
start

	process := [self runLoop]
		forkAt: self writingPriority
		named: 'RsrWriting runLoop'