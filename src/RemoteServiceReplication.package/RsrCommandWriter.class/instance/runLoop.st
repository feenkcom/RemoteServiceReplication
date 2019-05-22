commands
runLoop

	[true]
		whileTrue:
			[queue next sendUsing: self.
			queue isEmpty
				ifTrue: [stream flush]]