Class {
	#name : 'RsrTokenReceiver',
	#superclass : 'RsrTokenExchange',
	#category : 'RemoteServiceReplication'
}

{ #category : 'performing' }
RsrTokenReceiver >> perform [
	"Send the token. Wait for confirmation."

	| receivedToken |
	receivedToken := self codec decode: self stream.
	receivedToken = self token
		ifTrue:
			[self codec
				encodeTokenAccepted: nil "RsrTokenAccepted new"
				onto: self stream]
		ifFalse:
			[self codec
				encodeTokenRejected: nil "RsrTokenRejected new"
				onto: self steam.
			RsrTokenExchangeFailed signal]
]
