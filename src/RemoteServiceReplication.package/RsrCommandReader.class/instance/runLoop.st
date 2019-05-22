commands
runLoop

	[true]
		whileTrue:
			[[self nextCommand executeFor: connection]
				on: Exception
				do: [:ex | Transcript show: ex printString; cr]]