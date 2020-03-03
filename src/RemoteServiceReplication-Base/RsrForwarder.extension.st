Extension { #name : #RsrForwarder }

{ #category : #'*RemoteServiceReplication-Base' }
RsrForwarder >> _object: anRsrObject [

	_object := anRsrObject
]

{ #category : #'*RemoteServiceReplication-Base' }
RsrForwarder >> doesNotUnderstand: aMessage [

	| promise |
	promise := _object _connection
		_sendMessage: aMessage
		to: _object.
	^promise value
]

{ #category : #'*RemoteServiceReplication-Base' }
RsrForwarder class >> on: anRsrObject [

	| instance |
	instance := self new.
	instance _object: anRsrObject.
	^instance
]
