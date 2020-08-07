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

	| chunkSize bytes position numRead |
	chunkSize := 4096.
	bytes := ByteArray new: count.
	position := 1.
	[position <= count]
		whileTrue:
			[numRead := socket
				read: (chunkSize min: count - position + 1)
				into: bytes
				startingAt: position.
			position := position + numRead].
	^bytes
]

{ #category : 'adding' }
RsrSocketStream2 >> nextPutAll: bytes [
	"Write <bytes> to the socket."

	| chunkSize position numBytes numWritten |
	chunkSize := 4096.
	position := 1.
	numBytes := bytes size.
	[position <= numBytes]
		whileTrue:
			[numWritten := socket
				write: (chunkSize min: numBytes - position + 1)
				from: bytes
				startingAt: position.
			position := position + numWritten]
]

{ #category : 'accessing' }
RsrSocketStream2 >> socket: anRsrSocket [

	socket := anRsrSocket
]
