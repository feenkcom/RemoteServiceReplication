accessing
remoteServiceClass

	^self isClient
		ifTrue: [self class serverClass]
		ifFalse: [self class clientClass]