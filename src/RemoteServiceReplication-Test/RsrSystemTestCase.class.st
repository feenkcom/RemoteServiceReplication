Class {
	#name : 'RsrSystemTestCase',
	#superclass : 'RsrTestCase',
	#instVars : [
		'connectionA',
		'connectionB'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'testing' }
RsrSystemTestCase class >> isAbstract [

	^self == RsrSystemTestCase
]

{ #category : 'initialization' }
RsrSystemTestCase >> initializeInMemoryConnections [

	| aQueue bQueue channelA channelB |
	aQueue := SharedQueue new.
	bQueue := SharedQueue new.
	channelA := RsrInMemoryChannel
		inQueue: aQueue
		outQueue: bQueue.
	channelB := RsrInMemoryChannel
		inQueue: bQueue
		outQueue: aQueue.
	connectionA := RsrConnection
		channel: channelA
		transactionSpigot: RsrThreadSafeNumericSpigot naturals
		oidSpigot: RsrThreadSafeNumericSpigot naturals.
	connectionB := RsrConnection
		channel: channelB
		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated
		oidSpigot: RsrThreadSafeNumericSpigot naturals negated.
	channelA open.
	channelB open.
	self
		assert: connectionA isOpen;
		assert: connectionB isOpen
]

{ #category : 'initialization' }
RsrSystemTestCase >> initializeSocketConnections [

	| port semaphore |
	super setUp.
	port := 64455.
	semaphore := Semaphore new.
	self
		fork: [[connectionA := (RsrAcceptConnection port: port) waitForConnection] ensure: [semaphore signal]];
		fork: [[connectionB := (RsrInitiateConnection host: '127.0.0.1' port: port) connect] ensure: [semaphore signal]].
	semaphore wait; wait.
	self
		assert: connectionA isOpen;
		assert: connectionB isOpen
]

{ #category : 'accessing' }
RsrSystemTestCase >> serviceFactoryA [

	^connectionA serviceFactory
]

{ #category : 'accessing' }
RsrSystemTestCase >> serviceFactoryB [

	^connectionB serviceFactory
]

{ #category : 'initialization' }
RsrSystemTestCase >> setUp [
	"Subclasses need to start their connections by calling
	#initializeInMemoryConnections or #initializeSocketConnections.
	#tearDown will close connections."

	super setUp
]

{ #category : 'initialization' }
RsrSystemTestCase >> tearDown [

	connectionA ifNotNil: [:conn | conn close].
	connectionB ifNotNil: [:conn | conn close].
	connectionA := connectionB := nil.
	super tearDown
]
