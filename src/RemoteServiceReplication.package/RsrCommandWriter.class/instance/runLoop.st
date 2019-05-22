commands
runLoop

	[true]
		whileTrue:
			[[queue next sendUsing: self]
				on: Exception
				do: [:ex | Transcript show: ex printString; cr. self dumpStack. ex pass].
			queue isEmpty
				ifTrue: [stream flush]]