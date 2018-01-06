accessing
handleConnection
	|interactionSocket |
	interactionSocket := connectionSocket 
									waitForAcceptFor: (self timeout) 
									ifTimedOut: [ ^self ].
	[  self receiveDataUsing: interactionSocket ] 
	forkAt: 
		Processor highIOPriority 
		named: self printString  .
