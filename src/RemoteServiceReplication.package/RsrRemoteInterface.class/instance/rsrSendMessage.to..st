private-sending
rsrSendMessage: aMessage
to: aService

"Open coordination window"
	"Send dirty transitive closure of aRemoteMessage"
	"Send DispatchMessage command"
"Coorination window closed"
	"Return Promise"
	| dispatchCommand |
	connection isOpen
		ifFalse: [self error: 'Connection is not open'].
	dispatchCommand := RsrSendMessage
		transaction: self newTransactionId
		receiver: aService
		selector: aMessage selector
		arguments: aMessage arguments.
	^dispatchCommand sendOver: connection