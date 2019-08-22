run
RsrObject
	subclass: #RsrSocket
	instVarNames: #(socket)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

set class RsrSocket

classmethod:
new

	^super new initialize
%

classmethod:
on: aSocket

	^self new
		socket: aSocket;
		yourself
%

method:
socket: aSocket

	socket := aSocket
%

method:
initialize

	socket := GsSignalingSocket new
%

method:
accept

	^self class on: socket accept
%

method:
close

	socket close
%

method:
connectTo: aPort
on: aHostname

	^socket
		connectTo: aPort
		on: aHostname
%

method:
isConnected

	^socket isConnected
%

method:
listenOn: aPort

	socket
		makeServer: 2
		atPort: aPort
%

method:
read: aCount

	| bytes bytesRead |
	bytes := ByteArray new: aCount.
	bytesRead := socket
		read: aCount
		into: bytes.
	bytesRead = aCount
		ifFalse:
			[socket close. "GemStone doesn't seem to detect a socket closing"
			RsrConnectionClosed signal].
	^bytes
%

method:
write: bytes

	socket write: bytes
%