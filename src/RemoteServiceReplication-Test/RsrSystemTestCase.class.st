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

{ #category : 'initialization' }
RsrSystemTestCase >> tearDown [

	connectionA ifNotNil: [:conn | conn close].
	connectionB ifNotNil: [:conn | conn close].
	connectionA := connectionB := nil.
	super tearDown
]
