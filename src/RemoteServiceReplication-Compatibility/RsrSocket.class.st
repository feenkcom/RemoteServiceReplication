Class {
	#name : #RsrSocket,
	#superclass : #Object,
	#instVars : [
		'socket'
	],
	#category : #'RemoteServiceReplication-Compatibility'
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
		ifFalse: [^RsrError signal: 'Failed to read enough bytes'].
	^bytes
]

{ #category : #accessing }
RsrSocket >> socket: aSocket [

	socket := aSocket
]

{ #category : #reading }
RsrSocket >> waitForConnectionFor: seconds [

	socket waitForConnectionFor: seconds
]

{ #category : #reading }
RsrSocket >> write: bytes [

	socket sendData: bytes
]
