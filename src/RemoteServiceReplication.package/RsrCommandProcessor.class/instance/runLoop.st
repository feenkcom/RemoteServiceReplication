accessing
runLoop

	[isRunning]
		whileTrue:
			[[self executeCycle]
				on: Error
				do: [:ex | self stop]]