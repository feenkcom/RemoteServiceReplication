commands
start

	process := [self runLoop]
		forkAt: self priority
		named: self class name asString, ' loop'