Class {
	#name : 'RsrSocketStream2',
	#superclass : 'RsrObject',
	#instVars : [
		'socket'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrSocketStream2 class >> on: anRsrSocket [

	^self new
		socket: anRsrSocket;
		yourself
]

{ #category : 'testing' }
RsrSocketStream2 >> atEnd [
	"Return whether additional bytes could become available on the socket."

	^socket isConnected not
]

{ #category : 'closing' }
RsrSocketStream2 >> close [

	socket close
]

{ #category : 'flushing' }
RsrSocketStream2 >> flush [
	"Flush any buffered bytes to the socket."
	"NOP"
]

{ #category : 'testing' }
RsrSocketStream2 >> isConnected [
	"Is the stream still connected to a partner?"

	^socket isConnected
]

{ #category : 'accessing' }
RsrSocketStream2 >> next [
	"Return the next byte"

	^self next: 1
]

{ #category : 'accessing' }
RsrSocketStream2 >> next: count [
	"Return exactly <count> number of bytes.
	Signal RsrSocketClosed if the socket closes."

	| bytes |
	bytes := ByteArray new: count.
	socket
		read: count
		into: bytes
		startingAt: 1.
	^bytes
]

{ #category : 'adding' }
RsrSocketStream2 >> nextPutAll: bytes [
	"Write <bytes> to the socket."

	socket
		write: bytes size
		from: bytes
		startingAt: 1
]

{ #category : 'accessing' }
RsrSocketStream2 >> socket: anRsrSocket [

	socket := anRsrSocket
]
