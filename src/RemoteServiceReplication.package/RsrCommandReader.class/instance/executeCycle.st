commands
executeCycle

	[self nextCommand executeFor: connection]
		on: Exception
		do: [:ex | self stop]