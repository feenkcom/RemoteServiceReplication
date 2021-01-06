Class {
	#name : 'RsrTokenSender',
	#superclass : 'RsrTokenExchange',
	#category : 'RemoteServiceReplication'
}

{ #category : 'performing' }
RsrTokenSender >> perform [
	"Send the token. Wait for confirmation."

	| confirmation |
	self codec
		encodeToken: self token
		onto: self stream.
	confirmation := self codec decode: self stream.
	confirmation wasAccepted
		ifFalse: [RsrTokenExchangeFailed signal: 'Token was rejected']
]
