accessing
start
	isRunningLock critical: [ 
		self isRunning ifTrue: [ ^self ].
		self isRunning: true].
	
	connectionSocket := Socket newTCP.
	connectionSocket 
		listenOn: self port 
		backlogSize: self backlogSize.
	
   [ self listenLoop ]  fork.
	self debug 
		ifTrue: [Transcript clear ; show: 'Server started' ; cr].
	