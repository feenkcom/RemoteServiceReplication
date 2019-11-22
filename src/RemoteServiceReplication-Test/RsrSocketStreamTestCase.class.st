Class {
	#name : #RsrSocketStreamTestCase,
	#superclass : #RsrTestCase,
	#instVars : [
		'serverStream',
		'clientStream'
	],
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #initialization }
RsrSocketStreamTestCase >> initializeStreams [

	| listener client server |
	listener := RsrSocket new.
	client := RsrSocket new.
	listener listenOn: self listenPort.
	client connectTo: self listenPort on: '127.0.0.1'.
	server := listener accept.
	listener close.
	serverStream := RsrSocketStream on: server.
	clientStream := RsrSocketStream on: client
]

{ #category : #accessing }
RsrSocketStreamTestCase >> listenPort [

	^47856
]

{ #category : #running }
RsrSocketStreamTestCase >> setUp [

	super setUp.
	self initializeStreams
]

{ #category : #running }
RsrSocketStreamTestCase >> tearDown [

	serverStream close.
	clientStream close.
	super tearDown
]

{ #category : #running }
RsrSocketStreamTestCase >> testClose [

	serverStream close.
	self
		deny: serverStream isConnected;
		assert: clientStream isConnected.
	self
		should: [clientStream next]
		raise: RsrSocketClosed.
	self
		deny: clientStream isConnected
]
