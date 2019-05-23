commands
executeCycle

	[queue next sendUsing: self]
		on: Exception
		do: [:ex | self stop].
	queue isEmpty
		ifTrue: [self stream flush]