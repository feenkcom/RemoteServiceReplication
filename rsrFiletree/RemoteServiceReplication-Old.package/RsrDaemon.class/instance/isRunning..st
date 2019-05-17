accessing
isRunning: aBoolean
	isRunningLock critical: [ isRunning := aBoolean  ].