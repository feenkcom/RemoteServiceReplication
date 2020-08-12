Class {
	#name : 'RsrSocket',
	#superclass : 'RsrObject',
	#instVars : [
		'nativeSocket'
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
	nativeSocket := aSocket
]

{ #category : 'listening' }
RsrSocket >> accept [

	| timeoutHandler ns |
	timeoutHandler :=
		[nativeSocket isValid
			ifTrue: [nil]
			ifFalse: [RsrSocketClosed signal: 'Socket closed during #accept']].
	[ns isNil]
		whileTrue:
			[ns := nativeSocket
				waitForAcceptFor: 5
				ifTimedOut: timeoutHandler].
	^self class _nativeSocket: ns
]

{ #category : 'accessing' }
RsrSocket >> acceptTimeout [

	^31536000 "One year -- effectively forever"
]

{ #category : 'accessing' }
RsrSocket >> assertConnected [

	nativeSocket isConnected
		ifFalse: [RsrSocketClosed signal]
]

{ #category : 'accepting connections' }
RsrSocket >> bindAddress: addressString
port: port [

	(port between: 0 and: 65535)
		ifFalse: [^RsrInvalidBind signal: 'Specified port (', port printString, ') is out of range.'].
	[| address |
	address := NetNameResolver addressForName: addressString.
	nativeSocket
		bindTo: address
		port: port]
			on: NameLookupFailure, PrimitiveFailed, InvalidSocketStatusException
			do: [:ex | ex resignalAs: (RsrInvalidBind new messageText: ex messageText)]
]

{ #category : 'lifecycle' }
RsrSocket >> close [

	nativeSocket closeAndDestroy
]

{ #category : 'opening connection' }
RsrSocket >> connectToHost: aHostname
port: aPort [

	[| hostAddress |
	hostAddress := NetNameResolver addressForName: aHostname.
	nativeSocket
		connectTo: hostAddress
		port: aPort
		waitForConnectionFor: 5]
			on: NameLookupFailure, ConnectionTimedOut, InvalidSocketStatusException
			do: [:ex | ex resignalAs: (RsrConnectFailed new messageText: ex messageText)]
]

{ #category : 'initialization' }
RsrSocket >> initialize [

	super initialize.
	nativeSocket := Socket new
]

{ #category : 'testing' }
RsrSocket >> isConnected [

	^nativeSocket isConnected
]

{ #category : 'listening' }
RsrSocket >> listen: backlog [

	nativeSocket
		listenOn: nativeSocket port
		backlogSize: (backlog max: 2) "Ensure a new socket is created"
]

{ #category : 'reading' }
RsrSocket >> read: aCount [

	| bytes bytesRead |
	bytes := ByteArray new: aCount.
	bytesRead := nativeSocket receiveDataInto: bytes.
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
	numRead := nativeSocket receiveDataInto: tempBuffer.
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
	^nativeSocket
		sendSomeData: bytes
		startIndex: index
		count: count
]
