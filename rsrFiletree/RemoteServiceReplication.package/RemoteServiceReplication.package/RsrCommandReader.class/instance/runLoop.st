commands
runLoop

	[true]
		whileTrue:
			[self nextCommand executeFor: connection]