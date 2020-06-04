Class {
	#name : 'RsrSocketTestCase',
	#superclass : 'RsrTestCase',
	#category : 'RemoteServiceReplication-Platform-Test'
}

{ #category : 'running' }
RsrSocketTestCase >> assertWriting: bytes
to: writingSocket
isReadableOn: readSocket [

	| readBytes |
	writingSocket write: bytes.
	readBytes := readSocket read: bytes size.
	self
		assert: readBytes
		equals: bytes
]

{ #category : 'accessing' }
RsrSocketTestCase >> randomPort [

	^50123
]

{ #category : 'running' }
RsrSocketTestCase >> testConnectLocalSockets [

	| listener server client port |
	listener := RsrSocket new.
	client := RsrSocket new.
	port := self randomPort.
	listener listenOn: port.
	client
		connectToHost: '127.0.0.1'
		port: port.
	server := listener accept.
	listener close.
	self
		assert: server isConnected;
		assert: client isConnected.
	self
		assertWriting: #(1 2 3 4 5 6 7 8 9 0) asByteArray
		to: server
		isReadableOn: client.
	self
		assertWriting: #(0 9 8 7 6 5 4 3 2 1) asByteArray
		to: client
		isReadableOn: server
]

{ #category : 'running' }
RsrSocketTestCase >> testConnectToClosedPort [

	| socket |
	socket := RsrSocket new.
	self
		should: [socket connectToHost: '127.0.0.1' port: 64752]
		raise: Error
]

{ #category : 'running' }
RsrSocketTestCase >> testHasDataAvailable [

	| socketPair |
	socketPair := RsrSocketPair new.
	self deny: socketPair firstSocket dataAvailable.
	socketPair secondSocket write: #[1].
	self assert: socketPair firstSocket dataAvailable.
]

{ #category : 'running' }
RsrSocketTestCase >> testReadAvailable [

	| pair a b bytes readBytes writingProcess |
	pair := RsrSocketPair new.
	a := pair firstSocket.
	b := pair secondSocket.
	bytes := #[1].
	self deny: a dataAvailable.
	self
		assert: a readAvailable
		equals: #[].
	b write: bytes.
	self
		assert: a readAvailable
		equals: bytes.
	self deny: a dataAvailable.
	bytes := ByteArray withAll: (1 to: 255).
	b write: bytes.
	self
		assert: a readAvailable
		equals: bytes



]
