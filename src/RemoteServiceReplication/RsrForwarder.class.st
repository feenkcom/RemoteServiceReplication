Class {
	#name : #RsrForwarder,
	#superclass : #RsrProtoObject,
	#instVars : [
		'rsrObject'
	],
	#category : #'RemoteServiceReplication-Utilities'
}

{ #category : #'instance creation' }
RsrForwarder class >> on: anRsrObject [

	| instance |
	instance := self new.
	instance _object: anRsrObject.
	^instance
]

{ #category : #'private-accessing' }
RsrForwarder >> _object: anRsrObject [

	rsrObject := anRsrObject
]

{ #category : #'private-forwarding' }
RsrForwarder >> doesNotUnderstand: aMessage [

	| promise |
	promise := rsrObject _connection
		_sendMessage: aMessage
		to: rsrObject.
	^promise value
]
