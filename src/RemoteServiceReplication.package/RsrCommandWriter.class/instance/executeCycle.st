commands
executeCycle

	[queue next writeUsing: self.
	queue isEmpty
		ifTrue: [self flush]]
		on: ConnectionClosed, RsrConnectionClosed
		do: [:ex | self connection disconnected]