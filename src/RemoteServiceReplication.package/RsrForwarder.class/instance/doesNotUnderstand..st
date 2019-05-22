private-forwarding
doesNotUnderstand: aMessage

	| promise |
	promise := rsrObject rsrRemoteInterface
		rsrSendMessage: aMessage
		to: rsrObject.
	^promise value