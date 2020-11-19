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

	| spec |
	spec := RsrInMemoryConnectionSpecification new.
	spec connect.
	connectionA := spec connectionA.
	connectionB := spec connectionB.
	self
		assert: connectionA isOpen;
		assert: connectionB isOpen
]

{ #category : 'initialization' }
RsrSystemTestCase >> initializeSocketConnections [

	| spec |
	spec := RsrInternalSocketConnectionSpecification new.
	spec connect.
	connectionA := spec connectionA.
	connectionB := spec connectionB.
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
