| package |
package := Package name: 'RemoteServiceReplication-Dolphin'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #RsrAbstractService;
	add: #RsrClassResolver;
	add: #RsrEnvironment;
	add: #RsrForwarder;
	add: #RsrGarbageCollector;
	add: #RsrProtoObject;
	add: #RsrRegistryEntry;
	add: #RsrScientist;
	add: #RsrSocket;
	yourself.

package methodNames
	add: #Object -> #asString;
	add: #RsrCharacterArrayReference -> #convertBytes:;
	add: #RsrDoubleReference -> #convertBytes:;
	add: #RsrObject -> #initialize;
	add: #RsrProcessModel -> #currentStackDump;
	add: #SequenceableCollection -> #doWithIndex:;
	add: #Set -> #hash;
	add: 'RsrCharacterArrayReference class' -> #convertToBytes:;
	add: 'RsrDateAndTime class' -> #fromMicroseconds:;
	add: 'RsrDateAndTime class' -> #microsecondsSinceEpoch:;
	add: 'RsrDateAndTime class' -> #now;
	add: 'RsrDateAndTime class' -> #posixEpoch;
	add: 'RsrDoubleReference class' -> #convertToBytes:;
	add: 'RsrDoubleReference class' -> #infinity;
	add: 'RsrDoubleReference class' -> #nan;
	add: 'RsrObject class' -> #new;
	add: 'RsrReference class' -> #initializeReferenceMapping;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #('RemoteServiceReplication-Base').

package!

"Class Definitions"!

RsrObject subclass: #RsrAbstractService
	instanceVariableNames: 'finalizationSend'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrClassResolver
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrEnvironment
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrGarbageCollector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrRegistryEntry
	instanceVariableNames: 'storage strongReference finalizationAction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrScientist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrSocket
	instanceVariableNames: 'fd isConnected isBound'
	classVariableNames: ''
	poolDictionaries: 'WinSocketConstants'
	classInstanceVariableNames: ''!
ProtoObject subclass: #RsrProtoObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrProtoObject subclass: #RsrForwarder
	instanceVariableNames: '_service'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Object methodsFor!

asString

	^self printString! !
!Object categoriesFor: #asString!converting!public! !

!RsrCharacterArrayReference methodsFor!

convertBytes: aByteArray

	^Utf8String fromByteArray: aByteArray! !
!RsrCharacterArrayReference categoriesFor: #convertBytes:!public! !

!RsrCharacterArrayReference class methodsFor!

convertToBytes: aCharacterArray

	^aCharacterArray asUtf8String asByteArray! !
!RsrCharacterArrayReference class categoriesFor: #convertToBytes:!converting!public! !

!RsrDateAndTime class methodsFor!

fromMicroseconds: anInteger

	^TimeStamp fromMilliseconds: self posixEpoch asMilliseconds + (anInteger // 1000)!

microsecondsSinceEpoch: aTimeStamp

	| millisDiff |
	millisDiff := aTimeStamp asMilliseconds - self posixEpoch asMilliseconds.
	^millisDiff * 1000!

now

	^TimeStamp current!

posixEpoch

	^TimeStamp fromSeconds: 2177452800! !
!RsrDateAndTime class categoriesFor: #fromMicroseconds:!public! !
!RsrDateAndTime class categoriesFor: #microsecondsSinceEpoch:!public! !
!RsrDateAndTime class categoriesFor: #now!public! !
!RsrDateAndTime class categoriesFor: #posixEpoch!public! !

!RsrDoubleReference methodsFor!

convertBytes: bytes
	"I don't yet see an implementation for this method."

	^0.0! !
!RsrDoubleReference categoriesFor: #convertBytes:!public! !

!RsrDoubleReference class methodsFor!

convertToBytes: aFloat

	| integerBits bytes position |
	integerBits := aFloat bitRepresentation.
	bytes := ByteArray new: 8.
	position := 8.
	[position > 0]
		whileTrue:
			[bytes at: position put: (integerBits bitAnd: 16rFF).
			integerBits := integerBits bitShift: -8.
			position := position - 1].
	^bytes!

infinity

	^Float infinity!

nan

	^Float nan! !
!RsrDoubleReference class categoriesFor: #convertToBytes:!converting!public! !
!RsrDoubleReference class categoriesFor: #infinity!accessing!public! !
!RsrDoubleReference class categoriesFor: #nan!accessing!public! !

!RsrObject methodsFor!

initialize

	^self! !
!RsrObject categoriesFor: #initialize!public! !

!RsrObject class methodsFor!

new

	^super new initialize! !
!RsrObject class categoriesFor: #new!public! !

!RsrProcessModel methodsFor!

currentStackDump

	^Processor activeProcess stackTrace: 1000! !
!RsrProcessModel categoriesFor: #currentStackDump!public! !

!RsrReference class methodsFor!

initializeReferenceMapping
	"RsrReference initializeReferenceMapping"

	referenceMapping := Dictionary new.
	referenceMapping
		at: Symbol
		put: RsrSymbolReference.
	referenceMapping
		at: String
		put: RsrStringReference.
	referenceMapping
		at: Utf8String
		put: RsrStringReference.
	referenceMapping
		at: AnsiString
		put: RsrStringReference.
	referenceMapping
		at: LargeInteger
		put: RsrIntegerReference.
	referenceMapping
		at: SmallInteger
		put: RsrIntegerReference.
	referenceMapping
		at: Character
		put: RsrCharacterReference.
	referenceMapping
		at: UndefinedObject
		put: RsrNilReference.
	referenceMapping
		at: True
		put: RsrBooleanReference.
	referenceMapping
		at: False
		put: RsrBooleanReference.
	referenceMapping
		at: Array
		put: RsrArrayReference.
	referenceMapping
		at: ByteArray
		put: RsrByteArrayReference.
	referenceMapping
		at: Set
		put: RsrSetReference.
	referenceMapping
		at: OrderedCollection
		put: RsrOrderedCollectionReference.
	referenceMapping
		at: Dictionary
		put: RsrDictionaryReference.
	referenceMapping
		at: TimeStamp
		put: RsrDateAndTimeReference.
	referenceMapping
		at: Float
		put: RsrDoubleReference.
	^referenceMapping! !
!RsrReference class categoriesFor: #initializeReferenceMapping!public! !

!SequenceableCollection methodsFor!

doWithIndex: aBlock

	| index size |
	index := 1.
	size := self size.
	[index <= size]
		whileTrue:
			[aBlock
				value: (self at: index)
				value: index.
			index := index + 1]! !
!SequenceableCollection categoriesFor: #doWithIndex:!public! !

!Set methodsFor!

hash

	^self
		inject: #Set hash
		into: [:hash :each | hash bitXor: each hash]! !
!Set categoriesFor: #hash!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

RsrAbstractService guid: (GUID fromString: '{7fe58ad6-2d95-4e01-9243-9cb9d5506477}')!
RsrAbstractService comment: ''!
!RsrAbstractService categoriesForClass!Unclassified! !
!RsrAbstractService methodsFor!

finalize

	finalizationSend value!

toFinalizeEvaluate: aMessageSend

	finalizationSend := aMessageSend.
	self beFinalizable! !
!RsrAbstractService categoriesFor: #finalize!public! !
!RsrAbstractService categoriesFor: #toFinalizeEvaluate:!public! !

RsrClassResolver guid: (GUID fromString: '{bcecf4c2-9299-4982-b23b-1f72cc6e0c96}')!
RsrClassResolver comment: ''!
!RsrClassResolver categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrClassResolver class methodsFor!

classNamed: aSymbol

	^self
		classNamed: aSymbol
		ifAbsent: [RsrUnknownClass signal: aSymbol]!

classNamed: aSymbol
ifAbsent: aBlock

	^Smalltalk
		at: aSymbol
		ifAbsent: aBlock! !
!RsrClassResolver class categoriesFor: #classNamed:!public! !
!RsrClassResolver class categoriesFor: #classNamed:ifAbsent:!public! !

RsrEnvironment guid: (GUID fromString: '{d34e5cc4-a2e0-49ec-8483-17b8faccc9f0}')!
RsrEnvironment comment: ''!
!RsrEnvironment categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrEnvironment class methodsFor!

ifPharo: p
ifGemStone: g
ifDolphin: aBlock

	^aBlock value! !
!RsrEnvironment class categoriesFor: #ifPharo:ifGemStone:ifDolphin:!public! !

RsrGarbageCollector guid: (GUID fromString: '{6b2d74f4-36e4-45a4-95c8-e886a1855d81}')!
RsrGarbageCollector comment: ''!
!RsrGarbageCollector categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrGarbageCollector class methodsFor!

invokeGarbageCollector

	MemoryManager current collectGarbage!

maximumReclamation
	| service element sema didFinalize action |
	service := RsrAbstractService new.
	sema := Semaphore new.
	didFinalize := false.
	action := 
			[didFinalize := true.
			sema signal].
	element := RsrRegistryEntry
		service: service
		onMourn: action.
	service := nil.
	self invokeGarbageCollector.
	
	[(Delay forSeconds: 1) wait.	"Wait up to one second for finalization"
	sema signal] fork.
	sema wait.
	^didFinalize! !
!RsrGarbageCollector class categoriesFor: #invokeGarbageCollector!public! !
!RsrGarbageCollector class categoriesFor: #maximumReclamation!public! !

RsrRegistryEntry guid: (GUID fromString: '{636f9a23-234c-4a7a-ab6d-6d1700364857}')!
RsrRegistryEntry comment: ''!
!RsrRegistryEntry categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrRegistryEntry methodsFor!

becomeStrong

	strongReference := self service!

becomeWeak

	strongReference := nil!

elementsExpired: anInteger
of: anArray

	finalizationAction ifNotNil: [:action | [action value] fork]!

finalizationAction: anEvaluable

	finalizationAction := anEvaluable!

initialize

	super initialize.
	self initializeStorage!

initializeStorage

	storage := MourningWeakArray new: 1.
	storage pathologist: self!

service

	| service |
	service := storage at: 1.
	service == DeadObject current
		ifTrue: [^nil].
	^service!

service: aService

	storage
		at: 1
		put: aService! !
!RsrRegistryEntry categoriesFor: #becomeStrong!public! !
!RsrRegistryEntry categoriesFor: #becomeWeak!public! !
!RsrRegistryEntry categoriesFor: #elementsExpired:of:!public! !
!RsrRegistryEntry categoriesFor: #finalizationAction:!public! !
!RsrRegistryEntry categoriesFor: #initialize!public! !
!RsrRegistryEntry categoriesFor: #initializeStorage!public! !
!RsrRegistryEntry categoriesFor: #service!public! !
!RsrRegistryEntry categoriesFor: #service:!public! !

!RsrRegistryEntry class methodsFor!

service: aService
onMourn: aBlock

	^self new
		service: aService;
		finalizationAction: aBlock;
		yourself! !
!RsrRegistryEntry class categoriesFor: #service:onMourn:!public! !

RsrScientist guid: (GUID fromString: '{ca700baf-795f-44da-aef0-cc7d2ad19d68}')!
RsrScientist comment: ''!
!RsrScientist categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrScientist methodsFor!

instrument: aBlock
label: aString

	^aBlock value!

profile: aBlock
label: aString

	^aBlock value!

profile: aBlock
lable: aString
if: aCondition

	^aBlock value! !
!RsrScientist categoriesFor: #instrument:label:!public! !
!RsrScientist categoriesFor: #profile:label:!public! !
!RsrScientist categoriesFor: #profile:lable:if:!public! !

RsrSocket guid: (GUID fromString: '{401a4593-5b85-47ad-9860-48941a585902}')!
RsrSocket comment: 'This class implements the RsrSocket interface.

The implementation of this class is based upon SocketAbstract2, ServerSocket2, and Socket2.

Notes:
- This socket has not been tested against IPv6 yet.'!
!RsrSocket categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrSocket methodsFor!

_fd: aFileDescriptor
	"Private - Configure a connected socket from the provided descriptor"

	fd := aFileDescriptor.
	isConnected := true.
	isBound := false!

accept
	"Return an RsrSocket which is connected to a peer. In the event that the socket is closed while waiting, signal RsrSocketClosed."

	| address length newDescriptor |
	address := SOCKADDR_IN new.
	length := DWORD new
		value: address byteSize;
		yourself.
	newDescriptor := WS2_32Library default 
				accept: fd
				addr: address
				addrlen: length.
	newDescriptor = INVALID_SOCKET
		ifTrue: [self close. RsrSocketClosed signal: 'Socket closed during #accept'].
	^self class _fd: newDescriptor!

bindAddress: hostname
port: port
	"Bind the socket to the provided port and address. Signal RsrInvalidBind in the event the bind fails."

	| address ret |
	(port between: 0 and: 65535)
		ifFalse: [RsrInvalidBind signal: port asString, ' is not a valid port'].
	address := SOCKADDR_IN fromString: hostname.
	address
		sin_family: AF_INET;
		port: port.
	ret := WS2_32Library default
		bind: fd
		name: address
		namelen: address byteSize.
	ret = SOCKET_ERROR
		ifTrue: [^RsrInvalidBind signal: 'Unable to bind to ', hostname asString, ':', port asString].
	isBound := true!

close
	"Ensure closure of the Socket and cleanup any associated resources."

	isConnected := false.
	WS2_32Library default closesocket: fd!

connectToHost: hostname
port: port
	"Establish a connect to the provided host and port. If the socket is unable to establish, signal RsrConnectFailed.
	If the socket is bound to an address/port, signal RsrInvalidConnect.
	<hostname> - The name or ip address of a machine which should accept a connection.
	<port> - An integer representing a valid TCP port."

	| remoteAddress socketAddress result |
	(port between: 0 and: 65535)
		ifFalse: [^RsrConnectFailed signal: 'Invalid port specified: ', port asString].
	[remoteAddress := IN_ADDR address: (InternetAddress host: hostname)]
		on: SocketError
		do: [:ex | ex resignalAs: (RsrConnectFailed new messageText: ex messageText)].
	socketAddress := SOCKADDR_IN new
		sin_family: AF_INET;
		port: port;
		sin_addr: remoteAddress.
	result := WS2_32Library default
				connect: fd
				name: socketAddress
				namelen: socketAddress byteSize.
	result = SOCKET_ERROR
		ifTrue: [^RsrConnectFailed signal: 'Unable to connect to ', hostname asString, ':', port asString].
	isConnected := true!

initialize

	super initialize.
	isConnected := false.
	isBound := false.
	self initializeFileDescriptor!

initializeFileDescriptor
	"Private - Initialize the file descriptor used by this socket"

	| fileDescriptor |
	fileDescriptor := WS2_32Library default 
				socket: AF_INET
				type: SOCK_STREAM
				protocol: 0.
	fileDescriptor = INVALID_SOCKET
		ifTrue: [^RsrSocketError signal: 'Unable to initialize file descriptor'].
	fd := fileDescriptor.
	self beFinalizable!

isConnected
	"Return true if the socket is open and connected with a peer. Return false otherwise."

	^isConnected!

listen: backlogLength
	"Starting listening for connections. <backlogLength> specifies the number of connections to allow in a pending state.
	The actual backlog may support fewer prending connections depending upon implementation."

	| ret |
	isBound
		ifFalse: [self bindAddress: self wildcardAddress port: 0].
	ret := WS2_32Library default
		listen: fd
		backlog: backlogLength.
	ret = SOCKET_ERROR
		ifTrue: [^RsrSocketError signal: 'Failed to listen on port']!

port
	"Return the port associated with the socket."

	| name nl ret |
	name := (SOCKADDR_IN new)
				sin_family: AF_INET;
				yourself.
	nl := SDWORD new
		value: name size;
		yourself.
	ret := WS2_32Library default 
		getsockname: fd
		name: name
		namelen: nl.
	^ret = SOCKET_ERROR 
		ifTrue: [^0]
		ifFalse: [name port]!

read: count
into: bytes
startingAt: index
	"Read <count> number of bytes into <bytes> and place the first byte into slot <index>.
	<bytes> is assumed to be at least <count + index> bytes in size.
	Return the number of bytes successfully read. Signal RsrSocketClosed if the socket is closed before or during the call."

	| bytesReceived |
	bytesReceived := WS2_32Library default 
				recv: fd
				buf: bytes yourAddress + index - 1
				len: count
				flags: 0.
	bytesReceived > 0
		ifTrue: [^bytesReceived].
	bytesReceived = 0
		ifTrue:
			[self close.
			^RsrSocketClosed signal].
	^RsrSocketClosed signal!

wildcardAddress
	"Default bind address"

	^'0.0.0.0'!

write: count
from: bytes
startingAt: index
	"Write <count> number of bytes from <bytes> with <index> as the index of the first bytes.
	If <bytes> is smaller than <index + count> the behavior is undefined.
	If the socket is not connected, signal RsrSocketClosed."

	| result |
	result := WS2_32Library default 
				send: fd
				buf: bytes yourAddress + index - 1
				len: count
				flags: 0.
	result = SOCKET_ERROR
		ifTrue: [^RsrSocketClosed signal].
	^result! !
!RsrSocket categoriesFor: #_fd:!private! !
!RsrSocket categoriesFor: #accept!accepting connections!public! !
!RsrSocket categoriesFor: #bindAddress:port:!accepting connections!public! !
!RsrSocket categoriesFor: #close!public!terminating connections! !
!RsrSocket categoriesFor: #connectToHost:port:!establishing connections!public! !
!RsrSocket categoriesFor: #initialize!initialize/release!public! !
!RsrSocket categoriesFor: #initializeFileDescriptor!initialize/release!public! !
!RsrSocket categoriesFor: #isConnected!public!testing! !
!RsrSocket categoriesFor: #listen:!accepting connections!public! !
!RsrSocket categoriesFor: #port!accessing!public! !
!RsrSocket categoriesFor: #read:into:startingAt:!public!reading/writing! !
!RsrSocket categoriesFor: #wildcardAddress!accessing!public! !
!RsrSocket categoriesFor: #write:from:startingAt:!public!reading/writing! !

!RsrSocket class methodsFor!

_fd: aFileDescriptor
	"Private - Create a connected socket from the provided descriptor"

	^self basicNew
		_fd: aFileDescriptor;
		yourself! !
!RsrSocket class categoriesFor: #_fd:!private! !

RsrProtoObject guid: (GUID fromString: '{8c807a21-ab7c-4f1d-9c7a-f012b4953ad7}')!
RsrProtoObject comment: ''!
!RsrProtoObject categoriesForClass!RemoteServiceReplication-Dolphin! !
RsrForwarder guid: (GUID fromString: '{63d391a4-77f2-42aa-82a9-31cb9e9ed808}')!
RsrForwarder comment: ''!
!RsrForwarder categoriesForClass!Unclassified! !
!RsrForwarder methodsFor!

aspectDisplayOn: aStream

	aStream
		nextPutAll: 'RsrForwarder(';
		print: _service;
		nextPutAll: ')'!

class

	^RsrForwarder!

icon

	^self class icon!

newAspect: each
class: aspectClass

	^aspectClass name: each!

respondsTo: aSelector

	^self class canUnderstand: aSelector! !
!RsrForwarder categoriesFor: #aspectDisplayOn:!private! !
!RsrForwarder categoriesFor: #class!private! !
!RsrForwarder categoriesFor: #icon!private! !
!RsrForwarder categoriesFor: #newAspect:class:!private! !
!RsrForwarder categoriesFor: #respondsTo:!private! !

"Binary Globals"!

