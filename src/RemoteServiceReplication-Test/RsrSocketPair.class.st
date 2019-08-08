Class {
	#name : #RsrSocketPair,
	#superclass : #Object,
	#instVars : [
		'firstSocket',
		'secondSocket'
	],
	#category : #'RemoteServiceReplication-Test-Utilities'
}

{ #category : #'instance creation' }
RsrSocketPair class >> firstSocket: firstSocket
secondSocket: secondSocket [

	^super new
		firstSocket: firstSocket;
		secondSocket: secondSocket;
		yourself
]

{ #category : #accessing }
RsrSocketPair class >> listenPort [

	^64455
]

{ #category : #'instance creation' }
RsrSocketPair class >> new [

	| firstSocket secondSocket |
	firstSocket := RsrSocket new.
	secondSocket := RsrSocket new.
	firstSocket listenOn: self listenPort.
	secondSocket
		connectTo: NetNameResolver localHostAddress
		port: self listenPort.
	firstSocket waitForConnectionFor: self timeout.
	secondSocket waitForConnectionFor: self timeout.
	(firstSocket isConnected and: [secondSocket isConnected])
		ifFalse: [self error: 'Failed to create socket pair'].
	^self
		firstSocket: firstSocket
		secondSocket: secondSocket
]

{ #category : #accessing }
RsrSocketPair class >> timeout [

	^2
]

{ #category : #accessing }
RsrSocketPair >> close [

	firstSocket close.
	secondSocket close
]

{ #category : #accessing }
RsrSocketPair >> firstSocket [
	^ firstSocket
]

{ #category : #accessing }
RsrSocketPair >> firstSocket: anObject [
	firstSocket := anObject
]

{ #category : #accessing }
RsrSocketPair >> firstStream [

	^SocketStream on: firstSocket
]

{ #category : #accessing }
RsrSocketPair >> secondSocket [
	^ secondSocket
]

{ #category : #accessing }
RsrSocketPair >> secondSocket: anObject [
	secondSocket := anObject
]

{ #category : #accessing }
RsrSocketPair >> secondStream [

	^SocketStream on: secondSocket
]
