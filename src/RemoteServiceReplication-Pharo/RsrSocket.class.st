Class {
	#name : 'RsrSocket',
	#superclass : 'RsrObject',
	#instVars : [
		'socket'
	],
	#category : 'RemoteServiceReplication-Pharo'
}

{ #category : 'instance creation' }
RsrSocket class >> _nativeSocket: aSocket [
	"Private - Create a instance backed by the provided Pharo Socket"

	^self basicNew
		_nativeSocket: aSocket;
		yourself
]

{ #category : 'private-accessing' }
RsrSocket >> _nativeSocket: aSocket [
	"Private - Configure w/ a platform socket"

	aSocket setOption: 'TCP_NODELAY' value: 1.
	socket := aSocket
]

{ #category : 'listening' }
RsrSocket >> accept [

	^self class _nativeSocket: (socket waitForAcceptFor: self acceptTimeout ifTimedOut: [^nil])
]

{ #category : 'accessing' }
RsrSocket >> acceptTimeout [

	^31536000 "One year -- effectively forever"
]

{ #category : 'accessing' }
RsrSocket >> assertConnected [

	socket isConnected
		ifFalse: [RsrSocketClosed signal]
]

{ #category : 'accepting connections' }
RsrSocket >> bindAddress: addressString
port: port [

	| address |
	address := NetNameResolver addressForName: addressString.
	socket
		bindTo: address
		port: port
]

{ #category : 'lifecycle' }
RsrSocket >> close [

	socket closeAndDestroy
]

{ #category : 'opening connection' }
RsrSocket >> connectToHost: aHostname
port: aPort [

	[| hostAddress |
	hostAddress := NetNameResolver addressForName: aHostname.
	socket
		connectTo: hostAddress
		port: aPort
		waitForConnectionFor: 5]
			on: NameLookupFailure, ConnectionTimedOut, InvalidSocketStatusException
			do: [:ex | ex resignalAs: (RsrConnectFailed new messageText: ex messageText)]
]

{ #category : 'initialization' }
RsrSocket >> initialize [

	super initialize.
	socket := Socket new
]

{ #category : 'testing' }
RsrSocket >> isConnected [

	^socket isConnected
]

{ #category : 'listening' }
RsrSocket >> listen: backlog [

	socket
		listenOn: socket port
		backlogSize: (backlog max: 2) "Ensure a new socket is created"
]

{ #category : 'reading' }
RsrSocket >> read: aCount [

	| bytes bytesRead |
	bytes := ByteArray new: aCount.
	bytesRead := socket receiveDataInto: bytes.
	bytesRead = aCount
		ifFalse: [^RsrSocketClosed signal].
	^bytes
]

{ #category : 'reading' }
RsrSocket >> read: count
into: bytes
startingAt: index [ 

	| tempBuffer numRead |
	self assertConnected.
	tempBuffer := ByteArray new: count.
	numRead := socket receiveDataInto: tempBuffer.
	numRead = 0
		ifTrue: [RsrSocketClosed signal].
	bytes
		replaceFrom: index
		to: (index + count - 1)
		with: tempBuffer
		startingAt: 1.
	^numRead
]

{ #category : 'writing' }
RsrSocket >> write: count
from: bytes
startingAt: index [

	self assertConnected.
	^socket
		sendSomeData: bytes
		startIndex: index
		count: count
]
