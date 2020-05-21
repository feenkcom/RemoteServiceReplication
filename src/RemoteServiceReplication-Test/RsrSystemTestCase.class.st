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
RsrSystemTestCase >> setUp [

	| port semaphore |
	super setUp.
	port := 64455.
	semaphore := Semaphore new.
	self
		fork: [[connectionA := RsrConnection acceptOn: port] ensure: [semaphore signal]];
		fork: [[connectionB := RsrConnection connectTo: port on: '127.0.0.1'] ensure: [semaphore signal]].
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
