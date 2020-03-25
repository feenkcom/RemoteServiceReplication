Extension { #name : #RsrForwarder }

{ #category : #'*RemoteServiceReplication' }
RsrForwarder >> _service: aService [

	_service := aService
]

{ #category : #'*RemoteServiceReplication' }
RsrForwarder >> doesNotUnderstand: aMessage [

	| promise |
	promise := _service _connection
		_sendMessage: aMessage
		to: _service.
	^promise value
]

{ #category : #'*RemoteServiceReplication' }
RsrForwarder class >> on: anRsrObject [

	| instance |
	instance := self new.
	instance _service: anRsrObject.
	^instance
]
