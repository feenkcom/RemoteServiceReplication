Class {
	#name : #RsrSocket,
	#superclass : #RsrObject,
	#instVars : [
		'socket'
	],
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

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

	socket closeAndDestroy
]

{ #category : #'opening connection' }
RsrSocket >> connectTo: aPort
on: aHostname [

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
		ifFalse: [^RsrConnectionClosed signal].
	^bytes
]

{ #category : #accessing }
RsrSocket >> socket: aSocket [

	aSocket setOption: 'TCP_NODELAY' value: 1.
	socket := aSocket
]

{ #category : #reading }
RsrSocket >> write: bytes [

	socket sendData: bytes
]
