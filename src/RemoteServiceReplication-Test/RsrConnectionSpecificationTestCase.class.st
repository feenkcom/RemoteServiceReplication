Class {
	#name : 'RsrConnectionSpecificationTestCase',
	#superclass : 'RsrTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrConnectionSpecificationTestCase >> alternativeLocalhost [

	^'127.0.1.1'
]

{ #category : 'accessing' }
RsrConnectionSpecificationTestCase >> localhost [

	^'127.0.0.1'
]

{ #category : 'accessing' }
RsrConnectionSpecificationTestCase >> port [

	^47652
]

{ #category : 'running' }
RsrConnectionSpecificationTestCase >> testAcceptOnLocalhost [

	| acceptor initiator semaphore connectionA connectionB |
	acceptor := RsrAcceptConnection
		host: self localhost
		port: self port.
	initiator := RsrInitiateConnection
		host: self localhost
		port: self port.
	semaphore := Semaphore new.
	self
		fork: [[connectionA := acceptor waitForConnection] ensure: [semaphore signal]];
		fork: [[connectionB := initiator connect] ensure: [semaphore signal]].
	semaphore wait; wait.
	self
		assert: connectionA isOpen;
		assert: connectionB isOpen.
	connectionA close.
	connectionB close
]

{ #category : 'running' }
RsrConnectionSpecificationTestCase >> testCancelWaitForConnection [

	| acceptor |
	acceptor := RsrAcceptConnection port: self port.
	self fork: [(Delay forSeconds: 1) wait. acceptor cancelWaitForConnection].
	self
		should: [acceptor waitForConnection]
		raise: RsrWaitForConnectionCancelled
]

{ #category : 'running' }
RsrConnectionSpecificationTestCase >> testEstablishConnection [

	| acceptor initiator semaphore connectionA connectionB |
	acceptor := RsrAcceptConnection port: self port.
	initiator := RsrInitiateConnection
		host: self localhost
		port: self port.
	semaphore := Semaphore new.
	self
		fork: [[connectionA := acceptor waitForConnection] ensure: [semaphore signal]];
		fork: [[connectionB := initiator connect] ensure: [semaphore signal]].
	semaphore wait; wait.
	self
		assert: connectionA isOpen;
		assert: connectionB isOpen.
	connectionA close.
	connectionB close
]

{ #category : 'running' }
RsrConnectionSpecificationTestCase >> testFailedAcceptOnAlternativeLocalhost [

	| acceptor initiator semaphore |
	acceptor := RsrAcceptConnection
		host: self alternativeLocalhost
		port: self port.
	initiator := RsrInitiateConnection
		host: self localhost
		port: self port.
	semaphore := Semaphore new.
	self fork: [[semaphore signal. acceptor waitForConnection] on: RsrWaitForConnectionCancelled do: [:ex | ex return]].
	[semaphore wait.
	self
		should: [initiator connect]
		raise: RsrSocketError]
			ensure: [acceptor cancelWaitForConnection]
]
