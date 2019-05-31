Class {
	#name : #RsrForwarder,
	#superclass : #ProtoObject,
	#instVars : [
		'rsrObject'
	],
	#category : #'RemoteServiceReplication-Utilities'
}

{ #category : #'instance creation' }
RsrForwarder class >> on: anRsrObject [

	| instance |
	instance := self new.
	instance rsrObject: anRsrObject.
	^instance
]

{ #category : #'private-forwarding' }
RsrForwarder >> doesNotUnderstand: aMessage [

	| promise |
	promise := rsrObject rsrConnection
		rsrSendMessage: aMessage
		to: rsrObject.
	^promise value
]

{ #category : #'private-accessing' }
RsrForwarder >> rsrObject: anRsrObject [

	rsrObject := anRsrObject
]
