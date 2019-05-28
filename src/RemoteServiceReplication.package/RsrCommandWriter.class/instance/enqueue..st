commands
enqueue: aCommand

	isRunning ifTrue: [queue nextPut: aCommand]