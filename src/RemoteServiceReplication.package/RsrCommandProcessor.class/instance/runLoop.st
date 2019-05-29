running
runLoop

	[isRunning]
		whileTrue:
			[[self executeCycle]
				on: Error
				do: [:ex | self connection unknownError: ex]]