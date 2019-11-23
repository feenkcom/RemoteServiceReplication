RsrObject
	subclass: #RsrSocket
	instanceVariableNames: 'socket'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSocket categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

!RsrSocket class methodsFor!
on: aHostSocket

	^self new
		socket: aHostSocket;
		yourself! !

!RsrSocket methodsFor!
accept

	^self class on: socket accept!

close

	socket close!

connectTo: aPort
on: aHostname

	socket := Socket2
		port: aPort
		host: aHostname.
	socket connect!

dataAvailable

	^socket notNil and: [socket hasInput]!

isConnected

	^socket notNil and: [socket isOpen]!

listenOn: aPort

	socket := ServerSocket2
		port: aPort
		backlog: 1!

read: aCount

	| bytes |
	bytes := ByteArray new: aCount.
	[socket receive: bytes]
		on: SocketClosed
		do: [:ex | socket close.  ex resignalAs: RsrSocketClosed new].
	^bytes!

readAvailable

	| bytes totalBytesRead bytesRead |
	bytes := ByteArray new: 4096.
	totalBytesRead := 0.
	[self dataAvailable]
		whileTrue:
			[bytes size = totalBytesRead
				ifTrue: [bytes := bytes, (ByteArray new: 4096)].
			bytesRead := socket
				receiveSome: bytes
				count: bytes size - totalBytesRead
				startingAt: totalBytesRead + 1.
			totalBytesRead := totalBytesRead + bytesRead].
	^bytes copyFrom: 1 to: totalBytesRead!

socket: aHostSocket

	socket := aHostSocket!

write: aByteArray

	socket send: aByteArray! !
