Extension { #name : #RsrForwarder }

{ #category : #'*RemoteServiceReplication-Base' }
RsrForwarder class >> on: anRsrObject [

	| instance |
	instance := self new.
	instance _object: anRsrObject.
	^instance
]
