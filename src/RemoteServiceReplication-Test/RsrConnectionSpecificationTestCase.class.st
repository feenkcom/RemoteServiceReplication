Class {
	#name : 'RsrConnectionSpecificationTestCase',
	#superclass : 'RsrTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrConnectionSpecificationTestCase >> localhost [

	^'127.0.0.1'
]

{ #category : 'accessing' }
RsrConnectionSpecificationTestCase >> port [

	^47652
]

{ #category : 'running' }
RsrConnectionSpecificationTestCase >> testCancelWaitForConnection [

	| acceptor semaphore wasSignaled |
	acceptor := RsrAcceptConnection port: self port.
	semaphore := Semaphore new.
	wasSignaled := false.
	self
		fork:
			[semaphore signal.
			[acceptor waitForConnection]
				on: RsrWaitForConnectionCancelled
				do:
					[:ex |
					wasSignaled := true.
					semaphore signal.
					ex return]].
	self fork: [(Delay forSeconds: 3) wait. semaphore signal "ensure we don't ever deadlock the test"].
	semaphore wait. "Wait until forked block is running"
	acceptor cancelWaitForConnection.
	semaphore wait.
	self assert: wasSignaled.
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
