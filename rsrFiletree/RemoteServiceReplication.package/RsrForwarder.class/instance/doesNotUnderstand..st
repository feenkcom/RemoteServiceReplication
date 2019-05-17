private-forwarding
doesNotUnderstand: aMessage

	| remoteMessage |
	remoteMessage := RsrRemoteMessage
		receiver: rsrObject
		selector: aMessage selector
		arguments: aMessage arguments.
	^rsrObject rsrRemoteInterface send: remoteMessage