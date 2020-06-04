Class {
	#name : #RsrSocket,
	#superclass : #RsrObject,
	#instVars : [
		'socket'
	],
	#category : #'RemoteServiceReplication-Pharo'
}

{ #category : #'instance creation' }
RsrSocket class >> on: aSocket [

	^self new
		socket: aSocket;
		yourself
]

{ #category : #listening }
RsrSocket >> accept [

	^self class on: (socket waitForAcceptFor: self acceptTimeout ifTimedOut: [^nil])
]

{ #category : #accessing }
RsrSocket >> acceptTimeout [

	^1
]

{ #category : #lifecycle }
RsrSocket >> close [

	socket closeAndDestroy
]

{ #category : #'opening connection' }
RsrSocket >> connectToHost: aHostname
port: aPort [

	| hostAddress |
	hostAddress := NetNameResolver addressForName: aHostname.
	socket
		connectTo: hostAddress
		port: aPort
]

{ #category : #testing }
RsrSocket >> dataAvailable [

	^socket dataAvailable
]

{ #category : #initialization }
RsrSocket >> initialize [

	super initialize.
	socket := Socket new
]

{ #category : #testing }
RsrSocket >> isConnected [

	^socket isConnected
]

{ #category : #listening }
RsrSocket >> listenOn: aPort [

	socket listenOn: aPort backlogSize: 2
]

{ #category : #reading }
RsrSocket >> read: aCount [

	| bytes bytesRead |
	bytes := ByteArray new: aCount.
	bytesRead := socket receiveDataInto: bytes.
	bytesRead = aCount
		ifFalse: [^RsrSocketClosed signal].
	^bytes
]

{ #category : #reading }
RsrSocket >> readAvailable [

	| bytes bytesRead |
	bytes := ByteArray new: 4096.
	^ByteArray
		streamContents:
			[:stream |
			[self dataAvailable]
				whileTrue:
					[bytesRead := socket receiveAvailableDataInto: bytes.
					bytesRead == bytes size
						ifTrue: [stream nextPutAll: bytes]
						ifFalse: [stream nextPutAll: (bytes copyFrom: 1 to: bytesRead)]]]
]

{ #category : #accessing }
RsrSocket >> socket: aSocket [

	aSocket setOption: 'TCP_NODELAY' value: 1.
	socket := aSocket
]

{ #category : #writing }
RsrSocket >> write: bytes [

	socket sendData: bytes
]
