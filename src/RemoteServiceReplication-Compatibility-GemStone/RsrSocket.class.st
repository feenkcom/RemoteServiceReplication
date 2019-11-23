Class {
	#name : #RsrSocket,
	#superclass : #RsrObject,
	#instVars : [
		'socket'
	],
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #'instance creation' }
RsrSocket class >> new [

	^super new initialize
]

{ #category : #'instance creation' }
RsrSocket class >> on: aSocket [

	^self new
		socket: aSocket;
		yourself
]

{ #category : #listening }
RsrSocket >> accept [

	^self class on: socket accept
]

{ #category : #lifecycle }
RsrSocket >> close [

	socket close
]

{ #category : #'opening connection' }
RsrSocket >> connectTo: aPort
on: aHostname [

	^socket
		connectTo: aPort
		on: aHostname
]

{ #category : #testing }
RsrSocket >> dataAvailable [

	^socket readWillNotBlock
]

{ #category : #initialization }
RsrSocket >> initialize [

	super initialize.
	socket := GsSignalingSocket new
]

{ #category : #testing }
RsrSocket >> isConnected [

	^socket isConnected
]

{ #category : #listening }
RsrSocket >> listenOn: aPort [

	socket
		makeServer: 2
		atPort: aPort
]

{ #category : #reading }
RsrSocket >> read: aCount [

	| bytes bytesRead |
	bytes := ByteArray new: aCount.
	bytesRead := socket
		read: aCount
		into: bytes.
	bytesRead = aCount
		ifFalse:
			[socket close. "GemStone doesn't seem to detect a socket closing"
			RsrSocketClosed signal].
	^bytes
]

{ #category : #reading }
RsrSocket >> readAvailable [

	| bytes totalBytesRead bytesRead |
	bytes := ByteArray new: 4096.
	totalBytesRead := 0.
	[self dataAvailable]
		whileTrue:
			[bytes size = totalBytesRead
				ifTrue: [bytes := bytes, (ByteArray new: 4096)].
			bytesRead := socket
				read: bytes size - totalBytesRead
				into: bytes
				startingAt: totalBytesRead + 1.
			bytesRead = nil
				ifTrue: [^bytes copyFrom: 1 to: totalBytesRead].
			totalBytesRead := totalBytesRead + bytesRead].
	^bytes copyFrom: 1 to: totalBytesRead
]

{ #category : #accessing }
RsrSocket >> socket: aSocket [

	aSocket option: 'NODELAY' put: true.
	socket := aSocket
]

{ #category : #reading }
RsrSocket >> write: bytes [

	socket write: bytes
]
