commands
executeCycle

	queue next writeUsing: self.
	queue isEmpty
		ifTrue: [self stream flush]