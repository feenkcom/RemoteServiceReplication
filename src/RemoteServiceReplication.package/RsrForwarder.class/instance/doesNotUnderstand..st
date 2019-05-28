private-forwarding
doesNotUnderstand: aMessage

	| promise |
	promise := rsrObject rsrConnection
		rsrSendMessage: aMessage
		to: rsrObject.
	^promise value