RUN
Object
    subclass: 'RsrSocketStream'
    instVarNames: #('socket')
    classVars: #()
    classInstVars: #()
    poolDictionaries: #()
    inDictionary: UserGlobals
%

set class RsrSocketStream
set category 'Instance creation'

classmethod:
on: aSocket

    ^self new
        socket: aSocket;
        yourself
%

set category 'Accessing'

method:
socket: aSocket

    socket := aSocket
%

method:
socket

    ^socket
%

set category 'Stream Interfacing'

method:
flush

    "NOP"
%

method:
close

    self socket close
%

method:
nextPutAll: aByteArray

    self socket write: aByteArray
%

method:
next: aCount

    | bytes bytesRead |
    bytes := ByteArray new: aCount.
    bytesRead := self socket
        read: aCount
        into: bytes.
    bytesRead == aCount
        ifFalse: [^Error signal: 'Unexpected socket termination'].
    ^bytes
%