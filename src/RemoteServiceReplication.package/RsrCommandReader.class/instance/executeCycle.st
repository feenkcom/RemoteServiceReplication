commands
executeCycle

	[self dispatcher dispatch: self nextCommand]
		on: ConnectionClosed, RsrConnectionClosed
		do: [:ex | self connection disconnected]