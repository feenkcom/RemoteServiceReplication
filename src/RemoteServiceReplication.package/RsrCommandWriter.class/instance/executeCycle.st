commands
executeCycle

	queue next sendUsing: self.
	queue isEmpty
		ifTrue: [self stream flush]