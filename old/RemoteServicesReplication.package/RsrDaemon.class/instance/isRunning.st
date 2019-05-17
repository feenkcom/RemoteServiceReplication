testing
isRunning
	^ isRunningLock critical: [ isRunning  ]