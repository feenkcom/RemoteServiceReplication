dispatching
rsrSendMessage: aMessage
to: anRsrObject

	lastMessage := RsrSendMessage
		transaction: 1
		receiver: anRsrObject
		selector: aMessage selector
		arguments: aMessage arguments.
	^nil