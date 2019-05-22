private-forwarding
doesNotUnderstand: aMessage

	^rsrObject rsrRemoteInterface
		rsrSendMessage: aMessage
		to: rsrObject