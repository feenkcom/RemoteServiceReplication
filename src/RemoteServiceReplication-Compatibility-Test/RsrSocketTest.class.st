Class {
	#name : #RsrSocketTest,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Compatibility-Test'
}

{ #category : #running }
RsrSocketTest >> testConnectLocalSockets [

	| server client port sema |
	server := RsrSocket new.
	client := RsrSocket new.
	port := 7654.
	sema := Semaphore new.
	[server listenOn: port.
	sema signal] fork.
	[client connectTo: port on: '127.0.0.1'.
	sema signal] fork.
	sema wait; wait.
	self
		assert: server isConnected;
		assert: client isConnected
]

{ #category : #running }
RsrSocketTest >> testConnectToClosedPort [

	| socket |
	socket := RsrSocket new.
	self
		should: [socket connectTo: 64752 on: '127.0.0.1']
		raise: Error
]
