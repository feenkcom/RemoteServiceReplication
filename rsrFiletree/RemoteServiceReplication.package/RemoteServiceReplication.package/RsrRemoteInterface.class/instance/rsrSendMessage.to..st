private-sending
rsrSendMessage: aMessage
to: aService

"Open coordination window"
	"Send dirty transitive closure of aRemoteMessage"
	"Send DispatchMessage command"
"Coorination window closed"
	"Return Promise"
	| dispatchCommand |
	dispatchCommand := RsrSendMessage
		receiver: aService
		selector: aMessage
		arguments: aMessage arguments.
	
	self notYetImplemented