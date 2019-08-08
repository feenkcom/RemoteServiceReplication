Class {
	#name : #RsrSocket,
	#superclass : #Object,
	#instVars : [
		'socket'
	],
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #lifecycle }
RsrSocket >> close [

	socket close
]

{ #category : #'opening connection' }
RsrSocket >> connectTo: aHostname
port: aPortNumber [

	| hostAddress |
	hostAddress := NetNameResolver addressForName: aHostname.
	socket
		connectTo: hostAddress
		port: aPortNumber
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

	^socket listenOn: aPort
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

{ #category : #reading }
RsrSocket >> waitForConnectionFor: seconds [

	socket waitForConnectionFor: seconds
]

{ #category : #reading }
RsrSocket >> write: bytes [

	socket sendData: bytes
]
