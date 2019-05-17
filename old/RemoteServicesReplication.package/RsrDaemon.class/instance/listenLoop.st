accessing
listenLoop
	[[isRunning] 
		whileTrue: [ self handleConnection ]]
		ensure: [ connectionSocket closeAndDestroy. ]
	

	