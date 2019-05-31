Class {
	#name : #RsrSystemTestCase,
	#superclass : #RsrTestCase,
	#instVars : [
		'connectionA',
		'connectionB'
	],
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #testing }
RsrSystemTestCase class >> isAbstract [

	^self == RsrSystemTestCase
]

{ #category : #accesing }
RsrSystemTestCase >> serviceRegistryA [

	^connectionA serviceRegistry
]

{ #category : #accesing }
RsrSystemTestCase >> serviceRegistryB [

	^connectionB serviceRegistry
]

{ #category : #initialization }
RsrSystemTestCase >> setUp [

	| socketPair |
	super setUp.
	socketPair := RsrSocketPair new.
	connectionA := RsrConnection
		socket: socketPair firstSocket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals
		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1).
	connectionB := RsrConnection
		socket: socketPair secondSocket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated
		oidSpigot: RsrThreadSafeNumericSpigot naturals negated.
	connectionA start.
	connectionB start.
]

{ #category : #initialization }
RsrSystemTestCase >> tearDown [

	connectionA ifNotNil: [:conn | conn close].
	connectionB ifNotNil: [:conn | conn close].
	connectionA := connectionB := nil.
	super tearDown
]
