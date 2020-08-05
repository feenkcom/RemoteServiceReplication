Class {
	#name : 'RsrSocketStream2TestCase',
	#superclass : 'RsrTestCase',
	#instVars : [
		'aStream',
		'bStream'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'initializing' }
RsrSocketStream2TestCase >> initializeStreams [

	| socketPair |
	socketPair := RsrSocketPair2 new.
	aStream := socketPair firstStream.
	bStream := socketPair secondStream
]

{ #category : 'initializing' }
RsrSocketStream2TestCase >> setUp [

	super setUp.
	self initializeStreams
]

{ #category : 'initializing' }
RsrSocketStream2TestCase >> tearDown [

	aStream close.
	bStream close.
	super tearDown
]

{ #category : 'running' }
RsrSocketStream2TestCase >> testNextAfterClose [

	aStream close.
	self
		should: [aStream next]
		raise: RsrSocketClosed.
	self
		should: [bStream next]
		raise: RsrSocketClosed
]

{ #category : 'running' }
RsrSocketStream2TestCase >> testNextPutAllAfterClose [

	self deny: aStream atEnd.
	aStream close.
	self assert: aStream atEnd.
	self
		should: [aStream nextPutAll: #[1 2 3]]
		raise: RsrSocketClosed
]

{ #category : 'running' }
RsrSocketStream2TestCase >> testSendReceive [

	| count bytes |
	count := 1024.
	bytes := ByteArray new: count.
	aStream
		nextPutAll: bytes;
		flush.
	self
		assert: (bStream next: count)
		equals: bytes
]
