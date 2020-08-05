Class {
	#name : 'RsrSocketPair2',
	#superclass : 'RsrObject',
	#instVars : [
		'firstSocket',
		'secondSocket'
	],
	#category : 'RemoteServiceReplication-Platform-Test'
}

{ #category : 'instance creation' }
RsrSocketPair2 class >> firstSocket: firstSocket
secondSocket: secondSocket [

	^super new
		firstSocket: firstSocket;
		secondSocket: secondSocket;
		yourself
]

{ #category : 'accessing' }
RsrSocketPair2 class >> listenPort [

	^64455
]

{ #category : 'instance creation' }
RsrSocketPair2 class >> new [

	| localhost port listener firstSocket secondSocket |
	localhost := '127.0.0.1'.
	port := 8765.
	listener := self socketClass new.
	secondSocket := self socketClass new.
	listener
		bindAddress: localhost
		port: port.
	listener listen: 1.
	secondSocket
		connectToHost: localhost
		port: port.
	firstSocket := listener accept.
	listener close.
	(firstSocket isConnected and: [secondSocket isConnected])
		ifFalse: [self error: 'Failed to create socket pair'].
	^self
		firstSocket: firstSocket
		secondSocket: secondSocket
]

{ #category : 'accessing' }
RsrSocketPair2 class >> socketClass [

	^RsrSocket2
]

{ #category : 'closing' }
RsrSocketPair2 >> close [

	firstSocket close.
	secondSocket close
]

{ #category : 'accessing' }
RsrSocketPair2 >> firstSocket [

	^firstSocket
]

{ #category : 'accessing' }
RsrSocketPair2 >> firstSocket: anObject [

	firstSocket := anObject
]

{ #category : 'accessing' }
RsrSocketPair2 >> firstStream [

	^self socketStreamClass on: firstSocket
]

{ #category : 'accessing' }
RsrSocketPair2 >> secondSocket [

	^secondSocket
]

{ #category : 'accessing' }
RsrSocketPair2 >> secondSocket: anObject [

	secondSocket := anObject
]

{ #category : 'accessing' }
RsrSocketPair2 >> secondStream [

	^self socketStreamClass on: secondSocket
]

{ #category : 'accessing' }
RsrSocketPair2 >> socketStreamClass [

	^(RsrClassResolver classNamed: #RsrSocketStream2)
]
