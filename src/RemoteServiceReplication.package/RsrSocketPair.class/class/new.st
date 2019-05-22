instance creation
new

	| firstSocket secondSocket |
	firstSocket := Socket new.
	secondSocket := Socket new.
	firstSocket listenOn: self listenPort.
	secondSocket
		connectTo: NetNameResolver localHostAddress
		port: self listenPort.
	firstSocket waitForConnectionFor: self timeout.
	secondSocket waitForConnectionFor: self timeout.
	(firstSocket isConnected and: [secondSocket isConnected])
		ifFalse: [self error: 'Failed to create socket pair'].
	^self
		firstSocket: firstSocket
		secondSocket: secondSocket