| package |
package := Package name: 'RemoteServiceReplication'.
package paxVersion: 1; basicComment: ''.

package classNames
	add: #RsrInitiateConnection;
	add: #RsrSnapshotAnalysis;
	add: #RsrReleaseServices;
	add: #RsrAcceptConnection;
	add: #RsrRemoteError;
	add: #RsrPendingMessage;
	add: #RsrSocketChannelLoop;
	add: #RsrCommand;
	add: #RsrChannel;
	add: #RsrSocketStream;
	add: #RsrSendMessage;
	add: #RsrLogWithPrefix;
	add: #RsrCodec;
	add: #RsrLog;
	add: #RsrServiceFactory;
	add: #RsrCycleDetected;
	add: #RsrPromise;
	add: #RsrDeliverErrorResponse;
	add: #RsrSocketChannel;
	add: #RsrCommandSink;
	add: #RsrStream;
	add: #RsrInMemoryChannel;
	add: #RsrConnection;
	add: #RsrNumericSpigot;
	add: #RsrDecoder;
	add: #RsrLogSink;
	add: #RsrServiceFactoryClient;
	add: #RsrServiceSnapshot;
	add: #RsrDeliverResponse;
	add: #RsrTranscriptSink;
	add: #RsrNullChannel;
	add: #RsrCommandSource;
	add: #RsrService;
	add: #RsrBufferedSocketStream;
	add: #RsrConnectionSpecification;
	add: #RsrThreadSafeNumericSpigot;
	add: #RsrDispatchQueue;
	add: #RsrEncoder;
	add: #RsrCustomSink;
	add: #RsrServiceFactoryServer;
	yourself.

package methodNames
	add: #RsrForwarder -> #doesNotUnderstand:;
	add: #RsrForwarder -> #_service:;
	add: 'RsrReference class' -> #referenceClassFor:;
	add: 'RsrForwarder class' -> #on:;
	yourself.

package setPrerequisites: #('RemoteServiceReplication-Dolphin').

package!

RsrObject
	subclass: #RsrBufferedSocketStream
	instanceVariableNames: 'stream outBuffer writePosition nextToWrite'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrBufferedSocketStream categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrChannel
	instanceVariableNames: 'connection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrChannel comment: 'No class-specific documentation for RsrChannel, hierarchy is:Object  RsrObject    RsrChannel'!
!RsrChannel categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrCodec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCodec categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrCommand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrCommand comment: 'No class-specific documentation for RsrCommand, hierarchy is:Object  RsrObject    RsrCommand( encoding)'!
!RsrCommand categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrConnection
	instanceVariableNames: 'channel transactionSpigot oidSpigot dispatchQueue log registry pendingMessages serviceFactory closeSemaphore'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrConnection comment: 'No class-specific documentation for RsrConnection, hierarchy is:Object  RsrObject    RsrConnection( isOpen transactionSpigot commandWriter commandReader registry objectCache socket stream pendingMessages dispatcher oidSpigot serviceFactory log closeSemaphore)'!
!RsrConnection categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrConnectionSpecification
	instanceVariableNames: 'host port'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrConnectionSpecification comment: 'This class is abstract and defines the interface for manufacturing RsrConnection instances which are connected to a peer.Specialized subclasses are reponsible for either listening for or initiating connections with a peer.'!
!RsrConnectionSpecification categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrDispatchQueue
	instanceVariableNames: 'queue process isRunning'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrDispatchQueue comment: 'DispatchQueueThis class serves one purpose -- evaluate actions serially. Certain parts of the framework require this. For instance, Command processing needs to happen in the order it was received. (Note, this is not true of SendMessage commands which should fork the actual message send.)ProtectionsThis class should provide some low-level #on:do:. I don''t yet know what form this should take. I suspect it should coordinate w/ the Connection but I will leave this until I find an example error case.'!
!RsrDispatchQueue categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrLog
	instanceVariableNames: 'verbosity sinks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrLog categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrLogSink
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrLogSink categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrLogWithPrefix
	instanceVariableNames: 'prefix log'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrLogWithPrefix categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrNumericSpigot
	instanceVariableNames: 'current step'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrNumericSpigot categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrPendingMessage
	instanceVariableNames: 'services promise'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrPendingMessage categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrPromise
	instanceVariableNames: 'mutex value error markerValue'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrPromise categoriesForClass!RemoteServiceReplication! !

RsrAbstractService
	subclass: #RsrService
	instanceVariableNames: '_id _connection remoteSelf'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrService comment: 'I represent a class of Objects that know offer Rsr Services.'!
!RsrService categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrServiceSnapshot
	instanceVariableNames: 'sid template targetServiceType slots'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrServiceSnapshot comment: 'RsrServiceSnapshotWhen a SendMessage or DeliverResponse command is processed, the entire transition closure of the MessageSend/Response is analyzed.A Snapshot of each Service found during this process is taken. The slots of the Service that need to be replicated are stored in the ServiceSnapshot as references.In addition, information about the template and service is stored. This allows the peer to reify the correct type of Service. For instance, a local Client will be a Server remotely. A local Server will become a remote Client.Collaborators:- Encoder- Decoder- Reference'!
!RsrServiceSnapshot categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrSnapshotAnalysis
	instanceVariableNames: 'roots snapshots inFlight connection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSnapshotAnalysis comment: 'No class-specific documentation for RsrSnapshotAnalysis, hierarchy is:Object  RsrObject    RsrSnapshotAnalysis( roots snapshots inFlight connection)'!
!RsrSnapshotAnalysis categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrSocketChannelLoop
	instanceVariableNames: 'process channel state'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSocketChannelLoop comment: 'No class-specific documentation for RsrEventLoop, hierarchy is:Object  RsrObject    RsrEventLoop( process connection state)'!
!RsrSocketChannelLoop categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrSocketStream
	instanceVariableNames: 'socket'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSocketStream categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrStream
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrStream categoriesForClass!RemoteServiceReplication! !

RsrConnectionSpecification
	subclass: #RsrAcceptConnection
	instanceVariableNames: 'listener'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrAcceptConnection comment: 'This class is responsible to listen for an incoming RsrConnection connection. Once a Socket has established, an RsrConnection is created and returned via the #connect message.The following will wait for a connection on port 51820. Once a socket connection is accepted, it will stop listening on the provided port. The established socket is then used in the creation of an RsrConnection. The new RsrConnection is returned as a result of #connect.| acceptor |acceptor := RsrAcceptConnection port: 51820.^acceptor connect'!
!RsrAcceptConnection categoriesForClass!RemoteServiceReplication! !

RsrSocketChannelLoop
	subclass: #RsrCommandSink
	instanceVariableNames: 'queue'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrCommandSink comment: 'No class-specific documentation for RsrCommandSink, hierarchy is:Object  RsrObject    RsrEventLoop( process connection state)      RsrCommandSink( queue)'!
!RsrCommandSink categoriesForClass!RemoteServiceReplication! !

RsrSocketChannelLoop
	subclass: #RsrCommandSource
	instanceVariableNames: 'decoder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrCommandSource comment: 'Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:For the Class part:  State a one line summary. For example, "I represent a paragraph of text".For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.For the Collaborators Part: State my main collaborators and one line about how I interact with them. Public API and Key Messages- message one   - message two - (for bonus points) how to create instances.   One simple example is simply gorgeous. Internal Representation and Key Implementation Points.    Instance Variables	decoder:		<Object>    Implementation Points'!
!RsrCommandSource categoriesForClass!RemoteServiceReplication! !

RsrLogSink
	subclass: #RsrCustomSink
	instanceVariableNames: 'action'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCustomSink categoriesForClass!RemoteServiceReplication! !

RsrCodec
	subclass: #RsrDecoder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrDecoder comment: 'No class-specific documentation for RsrDecoder, hierarchy is:Object  RsrObject    RsrCodec      RsrDecoder( registry connection decodeCommandMap)'!
!RsrDecoder categoriesForClass!RemoteServiceReplication! !

RsrCommand
	subclass: #RsrDeliverErrorResponse
	instanceVariableNames: 'transaction originalClass remoteError'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrDeliverErrorResponse comment: 'No class-specific documentation for RsrDeliverErrorResponse, hierarchy is:Object  RsrObject    RsrCommand( encoding)      RsrDeliverErrorResponse( transaction originalClass remoteError)'!
!RsrDeliverErrorResponse categoriesForClass!RemoteServiceReplication! !

RsrCommand
	subclass: #RsrDeliverResponse
	instanceVariableNames: 'transaction response snapshots'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrDeliverResponse comment: 'No class-specific documentation for RsrDeliverResponse, hierarchy is:Object  RsrObject    RsrCommand( encoding)      RsrDeliverResponse( transaction response roots retainList)'!
!RsrDeliverResponse categoriesForClass!RemoteServiceReplication! !

RsrCodec
	subclass: #RsrEncoder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrEncoder categoriesForClass!RemoteServiceReplication! !

RsrChannel
	subclass: #RsrInMemoryChannel
	instanceVariableNames: 'inQueue outQueue drainProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrInMemoryChannel comment: 'Example usage:	| aQueue bQueue channelA channelB |	aQueue := SharedQueue new.	bQueue := SharedQueue new.	channelA := RsrInMemoryChannel		inQueue: aQueue		outQueue: bQueue.	channelB := RsrInMemoryChannel		inQueue: bQueue		outQueue: aQueue.	connectionA := RsrConnection		channel: channelA		transactionSpigot: RsrThreadSafeNumericSpigot naturals		oidSpigot: RsrThreadSafeNumericSpigot naturals.	connectionB := RsrConnection		channel: channelB		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated		oidSpigot: RsrThreadSafeNumericSpigot naturals negated.	connectionA open.	connectionB open.'!
!RsrInMemoryChannel categoriesForClass!RemoteServiceReplication! !

RsrConnectionSpecification
	subclass: #RsrInitiateConnection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrInitiateConnection comment: 'This class is responsible for initating a new RsrConnection. Sending #connect will result in an attempt to connect to the specified host and port. #connect is responsible for initating the attempted connection. If successful, an instance of RsrConnection is returned as a result.Example: | initiator |initiator := RsrInitiateConnection	host: ''127.0.0.1''	port: 51820.^initiator connect'!
!RsrInitiateConnection categoriesForClass!RemoteServiceReplication! !

RsrChannel
	subclass: #RsrNullChannel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrNullChannel categoriesForClass!RemoteServiceReplication! !

RsrCommand
	subclass: #RsrReleaseServices
	instanceVariableNames: 'sids'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrReleaseServices comment: 'No class-specific documentation for RsrReleaseServices, hierarchy is:Object  RsrObject    RsrCommand( encoding)      RsrReleaseServices( oids)'!
!RsrReleaseServices categoriesForClass!RemoteServiceReplication! !

RsrCommand
	subclass: #RsrSendMessage
	instanceVariableNames: 'transaction receiver selector arguments snapshots'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSendMessage comment: 'No class-specific documentation for RsrSendMessage, hierarchy is:Object  RsrObject    RsrCommand( encoding)      RsrSendMessage( transaction receiver selector arguments retainList)'!
!RsrSendMessage categoriesForClass!RemoteServiceReplication! !

RsrService
	subclass: #RsrServiceFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceFactory categoriesForClass!RemoteServiceReplication! !

RsrChannel
	subclass: #RsrSocketChannel
	instanceVariableNames: 'sink source socket stream'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSocketChannel comment: 'No class-specific documentation for RsrSocketChannel, hierarchy is:Object  RsrObject    RsrChannel      RsrSocketChannel( reader writer socket stream)'!
!RsrSocketChannel categoriesForClass!RemoteServiceReplication! !

RsrNumericSpigot
	subclass: #RsrThreadSafeNumericSpigot
	instanceVariableNames: 'mutex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrThreadSafeNumericSpigot categoriesForClass!RemoteServiceReplication! !

RsrLogSink
	subclass: #RsrTranscriptSink
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrTranscriptSink categoriesForClass!RemoteServiceReplication! !

RsrError
	subclass: #RsrCycleDetected
	instanceVariableNames: 'object'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCycleDetected categoriesForClass!RemoteServiceReplication! !

RsrError
	subclass: #RsrRemoteError
	instanceVariableNames: 'originalClassName stack'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRemoteError categoriesForClass!RemoteServiceReplication! !

RsrServiceFactory
	subclass: #RsrServiceFactoryClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceFactoryClient categoriesForClass!RemoteServiceReplication! !

RsrServiceFactory
	subclass: #RsrServiceFactoryServer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceFactoryServer categoriesForClass!RemoteServiceReplication! !

!RsrReference class methodsFor!
referenceClassFor: anObject	(anObject isKindOf: RsrService)		ifTrue: [^RsrServiceReference].	^self referenceMapping		at: anObject class		ifAbsent: [RsrUnsupportedObject signal: anObject]! !

!RsrForwarder class methodsFor!
on: anRsrObject	| instance |	instance := self new.	instance _service: anRsrObject.	^instance! !

!RsrForwarder methodsFor!
doesNotUnderstand: aMessage	| promise |	promise := _service _connection		_sendMessage: aMessage		to: _service.	^promise value! !

!RsrForwarder methodsFor!
_service: aService	_service := aService! !

!RsrSocketChannel class methodsFor!
socket: aSocket	^self new		socket: aSocket;		yourself! !

!RsrRemoteError class methodsFor!
from: anException	| tag |	tag := anException tag		ifNotNil:			[[anException tag asString]				on: Error				do: [:ex | ex return: 'Unable to pack #tag containing an instance of ', anException tag class name]].	^self new		originalClassName: anException class name;		tag: tag;		messageText: anException messageText;		stack: RsrProcessModel currentStackDump;		yourself! !

!RsrDeliverResponse class methodsFor!
transaction: aTransactionIdresponse: anObjectsnapshots: anArray	^self new		transaction: aTransactionId;		response: anObject;		snapshots: anArray;		yourself! !

!RsrDeliverResponse class methodsFor!
transaction: aTransactionIderror: anExceptionroots: anArray	^self new		transaction: aTransactionId;		errorName: anException class name;		response: anException messageText;		roots: anArray;		yourself! !

!RsrDeliverResponse class methodsFor!
transaction: aTransactionIdresponse: anObjectroots: anArray	^self new		transaction: aTransactionId;		response: anObject;		roots: anArray;		yourself! !

!RsrServiceFactory class methodsFor!
templateClassName	^#RsrServiceFactory! !

!RsrSocketChannelLoop class methodsFor!
on: aChannel	^self new		channel: aChannel;		yourself! !

!RsrService class methodsFor!
clientClassName	^(self templateClassName, 'Client') asSymbol! !

!RsrService class methodsFor!
isTemplateClass	^self name == self templateClassName! !

!RsrService class methodsFor!
_id: anIdconnection: aConnection	^super new		_id: anId connection: aConnection;		yourself! !

!RsrService class methodsFor!
isServerClass	^self name == self serverClassName! !

!RsrService class methodsFor!
isClientClass	^self name == self clientClassName! !

!RsrService class methodsFor!
serverClass	^RsrClassResolver classNamed: self serverClassName! !

!RsrService class methodsFor!
clientClass	^RsrClassResolver classNamed: self clientClassName! !

!RsrService class methodsFor!
serverClassName	^(self templateClassName, 'Server') asSymbol! !

!RsrService class methodsFor!
templateClass	^RsrClassResolver classNamed: self templateClassName! !

!RsrService class methodsFor!
templateClassName	self subclassResponsibility! !

!RsrServiceSnapshot class methodsFor!
reflectedVariablesFor: aServicedo: aBlock	self		reflectedVariableIndicesFor: aService		do: [:index | aBlock value: (aService instVarAt: index)]! !

!RsrServiceSnapshot class methodsFor!
reflectedVariablesFor: aService	| currentClass variables |	variables := OrderedCollection new.	currentClass := aService class templateClass.	[currentClass == RsrService]		whileFalse:			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].			currentClass := currentClass superclass].	^variables! !

!RsrServiceSnapshot class methodsFor!
reflectedVariableIndicesFor: aServicedo: aBlock	| allVariables |	allVariables := aService class allInstVarNames.	(self reflectedVariablesFor: aService)		do:			[:varName | | index |			index := allVariables indexOf: varName.			aBlock value: index]! !

!RsrServiceSnapshot class methodsFor!
from: aService	^self new		snapshot: aService;		yourself! !

!RsrSendMessage class methodsFor!
transaction: aTransactionIdreceiver: aServiceselector: aSelectorarguments: anArray	^self new		transaction: aTransactionId;		receiver: aService;		selector: aSelector;		arguments: anArray;		yourself! !

!RsrReleaseServices class methodsFor!
sids: anArrayOfServiceIDs	^self new		sids: anArrayOfServiceIDs;		yourself! !

!RsrLogWithPrefix class methodsFor!
prefix: aStringlog: aLog	^self new		prefix: aString;		log: aLog;		yourself! !

!RsrLogWithPrefix class methodsFor!
log: aLog	^self new		log: aLog;		yourself! !

!RsrBufferedSocketStream class methodsFor!
on: aSocketStream	^self new		stream: aSocketStream;		yourself! !

!RsrCustomSink class methodsFor!
action: aBlock	^self new		action: aBlock;		yourself! !

!RsrAcceptConnection class methodsFor!
wildcardAddress	^'0.0.0.0'! !

!RsrAcceptConnection class methodsFor!
port: aPortInteger	^self		host: self wildcardAddress		port: aPortInteger! !

!RsrPendingMessage class methodsFor!
services: aListpromise: aPromise	^self new		services: aList;		promise: aPromise;		yourself! !

!RsrInMemoryChannel class methodsFor!
inQueue: inQueueoutQueue: outQueue	^self new		inQueue: inQueue;		outQueue: outQueue;		yourself! !

!RsrStream class methodsFor!
on: aStream	^self new		stream: aStream;		yourself! !

!RsrCycleDetected class methodsFor!
signal: anObject	^self new		object: anObject;		signal! !

!RsrSnapshotAnalysis class methodsFor!
roots: anArrayconnection: aConnection	^self new		roots: anArray;		connection: aConnection;		yourself! !

!RsrConnection class methodsFor!
new	"Instances of Connection should not be created via #new.	Instead use ConnectionSpecification.	See SystemTestCase>>#setUp for an example."	self shouldNotImplement: #new! !

!RsrConnection class methodsFor!
channel: aChanneltransactionSpigot: aNumericSpigotoidSpigot: anOidSpigot	^super new		channel: aChannel;		transactionSpigot: aNumericSpigot;		oidSpigot: anOidSpigot;		yourself! !

!RsrDeliverErrorResponse class methodsFor!
transaction: aTransactionIdremoteError: anException	^self new		transaction: aTransactionId;		remoteError: anException;		yourself! !

!RsrConnectionSpecification class methodsFor!
host: hostnameOrAddressport: port	^self new		host: hostnameOrAddress;		port: port;		yourself! !

!RsrSocketStream class methodsFor!
on: anRsrSocket	^self new		socket: anRsrSocket;		yourself! !

!RsrDecoder class methodsFor!
registry: aRegistry	^self new		registry: aRegistry;		yourself! !

!RsrNumericSpigot class methodsFor!
new	^self		start: 0		step: 1! !

!RsrNumericSpigot class methodsFor!
naturals	^self		start: 1		step: 1! !

!RsrNumericSpigot class methodsFor!
start: aNumberstep: anIncrement	^super new		start: aNumber;		step: anIncrement;		yourself! !

!RsrNumericSpigot methodsFor!
step: anIncrement	step := anIncrement! !

!RsrNumericSpigot methodsFor!
step	^step! !

!RsrNumericSpigot methodsFor!
next	| result |	result := current.	current := current + step.	^result! !

!RsrNumericSpigot methodsFor!
next: aCount	| result |	result := Array new: aCount.	1 to: aCount do: [:i | result at: i put: self next].	^result! !

!RsrNumericSpigot methodsFor!
negated	^self class		start: current negated		step: step negated! !

!RsrNumericSpigot methodsFor!
start: aNumber	current := aNumber! !

!RsrReleaseServices methodsFor!
sids	^sids! !

!RsrReleaseServices methodsFor!
sids: anArrayOfServiceIDs	sids := anArrayOfServiceIDs! !

!RsrReleaseServices methodsFor!
executeFor: aConnection	sids do: [:sid | aConnection _remoteClientReleased: sid]! !

!RsrReleaseServices methodsFor!
reportOn: aLog	aLog debug: 'RsrReleaseObjects(', self sids printString, ')'! !

!RsrReleaseServices methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeReleaseServices: self		onto: aStream! !

!RsrSocketStream methodsFor!
isConnected	"Is the stream still connected to a partner?"	^socket isConnected! !

!RsrSocketStream methodsFor!
socket: anRsrSocket	socket := anRsrSocket! !

!RsrSocketStream methodsFor!
nextPutAll: bytes	"Write <bytes> to the socket."	| chunkSize position numBytes numWritten |	chunkSize := self chunkSize.	position := 1.	numBytes := bytes size.	[position <= numBytes]		whileTrue:			[numWritten := socket				write: (chunkSize min: numBytes - position + 1)				from: bytes				startingAt: position.			position := position + numWritten]! !

!RsrSocketStream methodsFor!
close	socket close! !

!RsrSocketStream methodsFor!
flush	"Flush any buffered bytes to the socket."	"NOP"! !

!RsrSocketStream methodsFor!
next	"Return the next byte"	^self next: 1! !

!RsrSocketStream methodsFor!
next: count	"Return exactly <count> number of bytes.	Signal RsrSocketClosed if the socket closes."	| chunkSize bytes position numRead |	chunkSize := self chunkSize.	bytes := ByteArray new: count.	position := 1.	[position <= count]		whileTrue:			[numRead := socket				read: (chunkSize min: count - position + 1)				into: bytes				startingAt: position.			position := position + numRead].	^bytes! !

!RsrSocketStream methodsFor!
chunkSize	"The largest size that should be read from or written to a Socket in each attempt."	^4096! !

!RsrSocketStream methodsFor!
atEnd	"Return whether additional bytes could become available on the socket."	^socket isConnected not! !

!RsrServiceFactoryServer methodsFor!
create: aResponsibility	| abstractClass |	abstractClass := RsrClassResolver classNamed: aResponsibility.	^abstractClass serverClass new! !

!RsrServiceFactoryServer methodsFor!
mirror: aService	^aService! !

!RsrCodec methodsFor!
deliverResponseCommand	^2! !

!RsrCodec methodsFor!
controlWordMin	^(2 raisedTo: 63) negated! !

!RsrCodec methodsFor!
controlWordMax	^(2 raisedTo: 63) - 1! !

!RsrCodec methodsFor!
deliverErrorResponseCommand	^4! !

!RsrCodec methodsFor!
immediateOID	^0! !

!RsrCodec methodsFor!
releaseObjectsCommand	^3! !

!RsrCodec methodsFor!
sendMessageCommand	^1! !

!RsrCodec methodsFor!
sizeOfInteger	"Return the number of bytes used to encode an integer"	^8! !

!RsrTranscriptSink methodsFor!
write: aMessageString	Transcript		show: aMessageString;		cr! !

!RsrPendingMessage methodsFor!
services	^services! !

!RsrPendingMessage methodsFor!
services: aList	services := aList! !

!RsrPendingMessage methodsFor!
promise	^promise! !

!RsrPendingMessage methodsFor!
promise: aPromise	promise := aPromise! !

!RsrStream methodsFor!
nextPutAll: aByteArray	^stream nextPutAll: aByteArray! !

!RsrStream methodsFor!
close	stream close! !

!RsrStream methodsFor!
next	^self next: 1! !

!RsrStream methodsFor!
next: aLength	| bytes |	bytes := stream next: aLength.	bytes size ~~ aLength		ifTrue: [RsrSocketClosed signal].	^bytes! !

!RsrStream methodsFor!
flush	stream flush! !

!RsrStream methodsFor!
binary	stream binary! !

!RsrStream methodsFor!
stream: aStream	stream := aStream! !

!RsrServiceFactoryClient methodsFor!
mirror: aService	^remoteSelf mirror: aService! !

!RsrServiceFactoryClient methodsFor!
serviceFor: aResponsibility	| abstractClass instance |	abstractClass := RsrClassResolver classNamed: aResponsibility.	instance := abstractClass clientClass new.	instance registerWith: _connection.	^instance! !

!RsrLogWithPrefix methodsFor!
prefix	^prefix! !

!RsrLogWithPrefix methodsFor!
log	^log! !

!RsrLogWithPrefix methodsFor!
log: aLog	log := aLog! !

!RsrLogWithPrefix methodsFor!
debug: aString	^self log debug: self prefix, '/', aString! !

!RsrLogWithPrefix methodsFor!
prefix: aString	prefix := aString! !

!RsrDeliverErrorResponse methodsFor!
transaction	^transaction! !

!RsrDeliverErrorResponse methodsFor!
reportOn: aLog	aLog debug: 'RsrDeliverErrorResponse(', self remoteError class name, ')'! !

!RsrDeliverErrorResponse methodsFor!
executeFor: aConnection	| pendingMessage |	pendingMessage := aConnection pendingMessages		removeKey: transaction		ifAbsent: [^self error: 'Handle unknown transaction'].	pendingMessage promise error: self remoteError! !

!RsrDeliverErrorResponse methodsFor!
remoteError	^remoteError! !

!RsrDeliverErrorResponse methodsFor!
remoteError: aRemoteError	remoteError := aRemoteError! !

!RsrDeliverErrorResponse methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeDeliverErrorResponse: self		onto: aStream! !

!RsrDeliverErrorResponse methodsFor!
transaction: anInteger	transaction := anInteger! !

!RsrCommandSource methodsFor!
nextCommand	^self decoder decodeCommand: self stream! !

!RsrCommandSource methodsFor!
executeCycle	[| command |	command := self nextCommand.	self report: command.	self channel received: command]		on: RsrSocketClosed		do:			[:ex |			self reportException: ex.			self channel channelDisconnected]! !

!RsrCommandSource methodsFor!
decoder	^RsrDecoder new! !

!RsrLogSink methodsFor!
write: aMessage	self subclassResponsibility! !

!RsrInMemoryChannel methodsFor!
drainLoop	| command |	[command := inQueue next.	command isNil]		whileFalse:			[command executeFor: self connection].	self connection channelDisconnected! !

!RsrInMemoryChannel methodsFor!
outQueue: aSharedQueue	outQueue := aSharedQueue! !

!RsrInMemoryChannel methodsFor!
close	outQueue nextPut: nil.	inQueue nextPut: nil! !

!RsrInMemoryChannel methodsFor!
inQueue: aSharedQueue	inQueue := aSharedQueue! !

!RsrInMemoryChannel methodsFor!
isOpen	^drainProcess isNil not! !

!RsrInMemoryChannel methodsFor!
open	drainProcess := RsrProcessModel fork: [self drainLoop. drainProcess := nil]! !

!RsrInMemoryChannel methodsFor!
send: aCommand	outQueue nextPut: aCommand! !

!RsrInMemoryChannel methodsFor!
inQueue	^inQueue! !

!RsrInMemoryChannel methodsFor!
outQueue	^outQueue! !

!RsrLog methodsFor!
levelError	^1! !

!RsrLog methodsFor!
levelInfo	^3! !

!RsrLog methodsFor!
debug: aString	self verbosity >= self levelDebug		ifTrue: [	self log: aString level: #debug]! !

!RsrLog methodsFor!
levelTrace	^5! !

!RsrLog methodsFor!
log: aMessagelevel: aLevelString	| message |	message := RsrDateAndTime now printString, '-', aLevelString, '-', aMessage.	sinks do: [:each | each write: message]! !

!RsrLog methodsFor!
levelDebug	^4! !

!RsrLog methodsFor!
verbosity	^verbosity! !

!RsrLog methodsFor!
initialize	super initialize.	verbosity := self levelTrace.	sinks := OrderedCollection new! !

!RsrLog methodsFor!
warning: aString	self verbosity >= self levelDebug		ifTrue: [self log: aString level: #warning]! !

!RsrLog methodsFor!
addSink: aLogSink	sinks add: aLogSink! !

!RsrLog methodsFor!
verbosity: aLogLevel	verbosity := aLogLevel! !

!RsrLog methodsFor!
info: aString	self verbosity >= self levelInfo		ifTrue: [self log: aString level: #info]! !

!RsrLog methodsFor!
levelWarn	^2! !

!RsrLog methodsFor!
trace: aString	self verbosity >= self levelTrace		ifTrue: [self log: aString level: #trace]! !

!RsrLog methodsFor!
error: aString	self verbosity >= self levelError		ifTrue: [self log: aString level: #error]! !

!RsrCommandSink methodsFor!
write: aByteArray	self stream nextPutAll: aByteArray! !

!RsrCommandSink methodsFor!
encoder	^RsrEncoder new! !

!RsrCommandSink methodsFor!
writeCommand: aCommand	self report: aCommand.	aCommand		encode: self stream		using: self encoder! !

!RsrCommandSink methodsFor!
flush	self stream flush! !

!RsrCommandSink methodsFor!
initialize	super initialize.	queue := SharedQueue new! !

!RsrCommandSink methodsFor!
enqueue: aCommand	self isActive ifTrue: [queue nextPut: aCommand]! !

!RsrCommandSink methodsFor!
stop	super stop.	queue nextPut: self stopToken! !

!RsrCommandSink methodsFor!
stopToken	^self stoppedState! !

!RsrCommandSink methodsFor!
executeCycle	[| command |	command := queue next.	command == self stopToken		ifTrue: [^self].	self writeCommand: command.	(queue size = 0)		ifTrue: [self flush]]		on: RsrSocketClosed		do:			[:ex |			self reportException: ex.			self channel channelDisconnected]! !

!RsrSocketChannelLoop methodsFor!
stop	self isActive ifFalse: [^self].	state := self stoppedState! !

!RsrSocketChannelLoop methodsFor!
log: aString	self log debug: aString! !

!RsrSocketChannelLoop methodsFor!
executeCycle	self subclassResponsibility! !

!RsrSocketChannelLoop methodsFor!
isProcessActive	^process ~~ nil! !

!RsrSocketChannelLoop methodsFor!
start	state := self runningState.	process := RsrProcessModel		fork: [self runLoop.				process := nil]		at: self priority! !

!RsrSocketChannelLoop methodsFor!
stoppedState	^#Stop! !

!RsrSocketChannelLoop methodsFor!
initialize	super initialize.	state := self stoppedState! !

!RsrSocketChannelLoop methodsFor!
priority	^Processor lowIOPriority! !

!RsrSocketChannelLoop methodsFor!
channel: aChannel	channel := aChannel! !

!RsrSocketChannelLoop methodsFor!
runningState	^#Running! !

!RsrSocketChannelLoop methodsFor!
report: aCommand	aCommand reportOn: self log! !

!RsrSocketChannelLoop methodsFor!
runLoop	[self isActive]		whileTrue:			[[self executeCycle]				on: Error				do:					[:ex |					self reportException: ex.					self channel genericError: ex]]! !

!RsrSocketChannelLoop methodsFor!
isActive	^state == self runningState! !

!RsrSocketChannelLoop methodsFor!
log	^RsrLogWithPrefix		prefix: self class name asString		log: self channel log! !

!RsrSocketChannelLoop methodsFor!
reportException: anException	self log: anException description! !

!RsrSocketChannelLoop methodsFor!
channel	^channel! !

!RsrSocketChannelLoop methodsFor!
stream	^self channel stream! !

!RsrSocketChannel methodsFor!
socket: aSocket	socket := aSocket! !

!RsrSocketChannel methodsFor!
close	"Shutdown the Command sink and source."	stream close.	source stop.	sink stop! !

!RsrSocketChannel methodsFor!
isOpen	^self socket isConnected! !

!RsrSocketChannel methodsFor!
socket	^socket! !

!RsrSocketChannel methodsFor!
send: aCommand	"Send the provided command over the channel"	sink enqueue: aCommand! !

!RsrSocketChannel methodsFor!
sink	^sink! !

!RsrSocketChannel methodsFor!
initialize	super initialize.	source := RsrCommandSource on: self.	sink := RsrCommandSink on: self! !

!RsrSocketChannel methodsFor!
disconnected	"The socket has disconnected so the channel is no longer open."	self connection channelDisconnected! !

!RsrSocketChannel methodsFor!
open	"Ensure the Command sink and source are running"	source start.	sink start! !

!RsrSocketChannel methodsFor!
stream	^stream ifNil: [stream := RsrSocketStream on: socket]! !

!RsrSocketChannel methodsFor!
source	^source! !

!RsrConnectionSpecification methodsFor!
port: aPort	"The port number used for establishing a socket"	port := aPort! !

!RsrConnectionSpecification methodsFor!
port	"The port number used for establishing a socket"	^port! !

!RsrConnectionSpecification methodsFor!
socketClass	"Return the class that should be used for creating Socket instances."	^RsrSocket! !

!RsrConnectionSpecification methodsFor!
host: hostnameOrAddress	"The hostname or IP address used to establish a connection."	host := hostnameOrAddress! !

!RsrConnectionSpecification methodsFor!
host	"Return the configured hostname or IP address"	^host! !

!RsrCustomSink methodsFor!
action: aBlock	action := aBlock! !

!RsrCustomSink methodsFor!
action	^action! !

!RsrCustomSink methodsFor!
write: aMessage	self action value: aMessage! !

!RsrSendMessage methodsFor!
receiver	^ receiver! !

!RsrSendMessage methodsFor!
snapshots	^snapshots! !

!RsrSendMessage methodsFor!
snapshots: anArrayOfSnapshots	snapshots := anArrayOfSnapshots! !

!RsrSendMessage methodsFor!
arguments	^ arguments! !

!RsrSendMessage methodsFor!
logException: anExceptionto: aLog	| message |	message := String		streamContents:			[:stream |			stream				print: receiver;				nextPutAll: '>>';				print: selector;				nextPutAll: ' due to: ';				nextPutAll: anException description].	aLog error: message! !

!RsrSendMessage methodsFor!
arguments: anObject	arguments := anObject! !

!RsrSendMessage methodsFor!
executeFor: aConnection	| result analysis resultReference response |	[| servs rec sel args |	servs := snapshots collect: [:each | each reifyIn: aConnection].	rec := receiver resolve: aConnection.	sel := selector resolve: aConnection.	args := arguments collect: [:each | each resolve: aConnection].	result := rec		perform: sel		withArguments: args.	analysis := RsrSnapshotAnalysis		roots: (Array with: rec with: result)		connection: aConnection.	analysis perform.	resultReference := RsrReference from: result.	response := RsrDeliverResponse		transaction: transaction		response: resultReference		snapshots: analysis snapshots.	aConnection _sendCommand: response]		on: Error		do:			[:ex |			self				logException: ex				to: aConnection log.			aConnection _sendCommand: (RsrDeliverErrorResponse transaction: transaction remoteError: (RsrRemoteError from: ex))]! !

!RsrSendMessage methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeSendMessage: self		onto: aStream! !

!RsrSendMessage methodsFor!
selector	^ selector! !

!RsrSendMessage methodsFor!
selector: anObject	selector := anObject! !

!RsrSendMessage methodsFor!
receiver: anObject	receiver := anObject! !

!RsrSendMessage methodsFor!
reportOn: aLog	aLog debug: 'RsrSendMessage(', self receiver asString, '>>', self selector asString, ')'! !

!RsrSendMessage methodsFor!
transaction	^ transaction! !

!RsrSendMessage methodsFor!
transaction: anObject	transaction := anObject! !

!RsrServiceSnapshot methodsFor!
snapshot: aService	sid := aService _id.	template := aService class templateClassName.	targetServiceType := aService isClient		ifTrue: [#server]		ifFalse: [#client].	slots := OrderedCollection new.	RsrServiceSnapshot		reflectedVariablesFor: aService		do: [:each | slots add: (RsrReference from: each)]! !

!RsrServiceSnapshot methodsFor!
slots: anArrayOfReferences	slots := anArrayOfReferences! !

!RsrServiceSnapshot methodsFor!
targetServiceType: aSymbol	targetServiceType := aSymbol! !

!RsrServiceSnapshot methodsFor!
template: aSymbol	template := aSymbol! !

!RsrServiceSnapshot methodsFor!
createBasicInstance	^self shouldCreateClient		ifTrue: [self templateClass clientClass basicNew]		ifFalse: [self templateClass serverClass basicNew]! !

!RsrServiceSnapshot methodsFor!
instanceIn: aConnection	^aConnection		serviceAt: self sid		ifAbsent:			[| instance |			instance := self createBasicInstance.			instance				_id: self sid				connection: aConnection.			aConnection				serviceAt: self sid				put: instance.			instance]! !

!RsrServiceSnapshot methodsFor!
slots	^slots! !

!RsrServiceSnapshot methodsFor!
snapshotIdentifier	^0! !

!RsrServiceSnapshot methodsFor!
decode: aStreamusing: aDecoder	| species instVarCount serviceName templateClass |	species := aDecoder decodeControlWord: aStream.	sid := aDecoder decodeControlWord: aStream.	instVarCount := aDecoder decodeControlWord: aStream.	serviceName := (aDecoder decodeReference: aStream) resolve: nil.	templateClass := (RsrClassResolver classNamed: serviceName) templateClass.	template := templateClass templateClassName.	targetServiceType := templateClass clientClassName = serviceName		ifTrue: [#client]		ifFalse: [#server].	slots := OrderedCollection new: instVarCount.	instVarCount timesRepeat: [slots add: (aDecoder decodeReference: aStream)]! !

!RsrServiceSnapshot methodsFor!
sid	^sid! !

!RsrServiceSnapshot methodsFor!
reifyIn: aConnection	| instance referenceStream |	instance := self instanceIn: aConnection.	(self class reflectedVariablesFor: instance) size = slots size		ifFalse: [self error: 'Incorrected encoded instance detected'].	referenceStream := ReadStream on: slots.	instance preUpdate.	self class		reflectedVariableIndicesFor: instance		do: [:index | instance instVarAt: index put: (referenceStream next resolve: aConnection)].	instance postUpdate.	^instance! !

!RsrServiceSnapshot methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeControlWord: self snapshotIdentifier		onto: aStream.	anEncoder		encodeControlWord: self sid		onto: aStream.	anEncoder		encodeControlWord: self slots size		onto: aStream.	self targetClassNameReference		encode: aStream		using: anEncoder.	self slots do: [:each | each encode: aStream using: anEncoder]! !

!RsrServiceSnapshot methodsFor!
targetClassNameReference	| targetClassName |	targetClassName := self shouldCreateClient		ifTrue: [self templateClass clientClassName]		ifFalse: [self templateClass serverClassName].	^RsrSymbolReference symbol: targetClassName! !

!RsrServiceSnapshot methodsFor!
shouldCreateClient	^self targetServiceType == #client! !

!RsrServiceSnapshot methodsFor!
sid: aServiceID	sid := aServiceID! !

!RsrServiceSnapshot methodsFor!
targetServiceType	^targetServiceType! !

!RsrServiceSnapshot methodsFor!
template	^template! !

!RsrServiceSnapshot methodsFor!
templateClass	^RsrClassResolver classNamed: self template! !

!RsrAcceptConnection methodsFor!
waitForConnection	| socket channel connection |	listener := self socketClass new.	[listener		bindAddress: self host		port: self port.	listener listen: 1.	socket := [listener accept]		on: RsrSocketClosed		do: [:ex | ex resignalAs: RsrWaitForConnectionCancelled new]]			ensure:				[listener close.				listener := nil].	channel := RsrSocketChannel socket: socket.	connection := RsrConnection		channel: channel		transactionSpigot: RsrThreadSafeNumericSpigot naturals		oidSpigot: RsrThreadSafeNumericSpigot naturals.	^connection open! !

!RsrAcceptConnection methodsFor!
cancelWaitForConnection	listener ifNotNil: [:socket | socket close]! !

!RsrAcceptConnection methodsFor!
isWaitingForConnection	^listener ~~ nil! !

!RsrDispatchQueue methodsFor!
async: aBlock	"Evaluate the block asynchronously and do not return a result"	queue nextPut: aBlock.	^nil! !

!RsrDispatchQueue methodsFor!
initialize	super initialize.	queue := SharedQueue new! !

!RsrDispatchQueue methodsFor!
runLoop	[self isRunning]		whileTrue:			[queue next value]! !

!RsrDispatchQueue methodsFor!
dispatch: aBlock	^self async: aBlock! !

!RsrDispatchQueue methodsFor!
stop	"Stop process events in the dispatch queue."	isRunning := false.	self dispatch: []. "Ensure another action is added to the queue to ensure shutdown if it hasn't yet happened."	process := nil! !

!RsrDispatchQueue methodsFor!
start	"Start processing queued events."	isRunning := true.	process := RsrProcessModel fork: [self runLoop]! !

!RsrDispatchQueue methodsFor!
isRunning	^isRunning! !

!RsrRemoteError methodsFor!
originalClassName	^originalClassName! !

!RsrRemoteError methodsFor!
stack: aString	stack := aString! !

!RsrRemoteError methodsFor!
originalClassName: aSymbol	originalClassName := aSymbol! !

!RsrRemoteError methodsFor!
stack	^stack! !

!RsrConnection methodsFor!
serviceAt: aSIDifAbsent: aBlock	^registry serviceAt: aSID ifAbsent: aBlock! !

!RsrConnection methodsFor!
serviceFor: aResponsibility	^self serviceFactory serviceFor: aResponsibility! !

!RsrConnection methodsFor!
close	channel close.	self dispatchQueue stop.	pendingMessages do: [:each | each promise error: RsrConnectionClosed new].	pendingMessages := Dictionary new.	registry := nil.	closeSemaphore signal! !

!RsrConnection methodsFor!
log	^log! !

!RsrConnection methodsFor!
initialize	super initialize.	transactionSpigot := RsrThreadSafeNumericSpigot naturals.	pendingMessages := Dictionary new.	registry := RsrRegistry reapAction: [:oid | self releaseOid: oid].	dispatchQueue := RsrDispatchQueue new.	log := RsrLog new.	closeSemaphore := Semaphore new.! !

!RsrConnection methodsFor!
_remoteClientReleased: aSID	"Remotely, a Client instance has been garbage collected.	Ensure we only reference the associated service weakly."	| entry |	entry := registry		_At: aSID		ifAbsent: [^self].	entry becomeWeak.! !

!RsrConnection methodsFor!
transactionSpigot	^transactionSpigot! !

!RsrConnection methodsFor!
channel: aChannel	channel := aChannel.	channel connection: self! !

!RsrConnection methodsFor!
oidSpigot	^oidSpigot! !

!RsrConnection methodsFor!
oidSpigot: anIntegerSpigot	oidSpigot := anIntegerSpigot! !

!RsrConnection methodsFor!
serviceAt: aSID	^registry serviceAt: aSID! !

!RsrConnection methodsFor!
_sendMessage: aMessageto: aService"Open coordination window"	"Send dirty transitive closure of aRemoteMessage"	"Send DispatchMessage command""Coorination window closed"	"Return Promise"	| analysis receiverReference selectorReference argumentReferences dispatchCommand promise pendingMessage |	self isOpen		ifFalse: [self error: 'Connection is not open'].	analysis := RsrSnapshotAnalysis		roots: (Array with: aService), aMessage arguments		connection: self.	analysis perform.	receiverReference := RsrReference from: aService.	selectorReference := RsrReference from: aMessage selector.	argumentReferences := aMessage arguments collect: [:each | RsrReference from: each].	dispatchCommand := RsrSendMessage		transaction: self transactionSpigot next		receiver: receiverReference		selector: selectorReference		arguments: argumentReferences.	dispatchCommand snapshots: analysis snapshots.	promise := RsrPromise new.	pendingMessage := RsrPendingMessage		services: nil "I don't think we need to cache services here. They will remain on the stack unless they were removed from the transitive closure by another proc"		promise: promise.	self pendingMessages		at: dispatchCommand transaction		put: pendingMessage.	self _sendCommand: dispatchCommand.	^promise! !

!RsrConnection methodsFor!
pendingMessages	^pendingMessages! !

!RsrConnection methodsFor!
unknownError: anException	self close! !

!RsrConnection methodsFor!
ensureRegistered: aService	aService isMirrored		ifTrue:			[^aService _connection == self				ifTrue: [self]				ifFalse: [RsrAlreadyRegistered signalService: aService intendedConnection: self]].	aService		_id: oidSpigot next		connection: self.	self		serviceAt: aService _id		put: aService! !

!RsrConnection methodsFor!
_sendCommand: aCommand	channel send: aCommand! !

!RsrConnection methodsFor!
initializeServiceFactory	| instance |	instance := RsrServiceFactory clientClass new.	self ensureRegistered: instance.	serviceFactory := instance.	^serviceFactory! !

!RsrConnection methodsFor!
waitUntilClose	closeSemaphore		wait;		signal! !

!RsrConnection methodsFor!
channelDisconnected	self log info: 'Disconnected'.	self close! !

!RsrConnection methodsFor!
serviceAt: aSIDput: aService	^registry		serviceAt: aSID		put: aService! !

!RsrConnection methodsFor!
releaseOid: anOid	| command |	self isOpen		ifFalse: [^self].	self log trace: 'Cleaning up OID:', anOid printString.	command := RsrReleaseServices sids: (Array with: anOid).	self _sendCommand: command! !

!RsrConnection methodsFor!
_forwarderClass	^RsrForwarder! !

!RsrConnection methodsFor!
serviceFactory	^serviceFactory ifNil: [self initializeServiceFactory]! !

!RsrConnection methodsFor!
open	self dispatchQueue start.	channel open! !

!RsrConnection methodsFor!
transactionSpigot: anObject	transactionSpigot := anObject! !

!RsrConnection methodsFor!
dispatchQueue	^dispatchQueue! !

!RsrConnection methodsFor!
isOpen	^channel isOpen! !

!RsrConnection methodsFor!
channel	^channel! !

!RsrDecoder methodsFor!
decodeReleaseServices: aStream	| count oids |	count := self decodeControlWord: aStream.	oids := Array new: count.	1		to: count		do:			[:i | | oid |			oid := self decodeControlWord: aStream.			oids at: i put: oid].	^RsrReleaseServices sids: oids! !

!RsrDecoder methodsFor!
decodeControlWord: aStream	| bytes unsignedResult |	bytes := aStream next: self sizeOfInteger.	unsignedResult := self bytesAsInteger: bytes.	^unsignedResult > self controlWordMax		ifTrue: [(2 raisedTo: 64) negated + unsignedResult]		ifFalse: [unsignedResult]! !

!RsrDecoder methodsFor!
instanceOfImmediate: aReferenceType	aReferenceType = 1		ifTrue: [^RsrSymbolReference new].	aReferenceType = 2		ifTrue: [^RsrStringReference new].	aReferenceType = 3		ifTrue: [^RsrPositiveIntegerReference new].	aReferenceType = 4		ifTrue: [^RsrNegativeIntegerReference new].	aReferenceType = 5		ifTrue: [^RsrCharacterReference new].	aReferenceType = 6		ifTrue: [^RsrNilReference new].	aReferenceType = 7		ifTrue: [^RsrTrueReference new].	aReferenceType = 8		ifTrue: [^RsrFalseReference new].	aReferenceType = 9		ifTrue: [^RsrArrayReference new].	aReferenceType = 10		ifTrue: [^RsrByteArrayReference new].	aReferenceType = 11		ifTrue: [^RsrSetReference new].	aReferenceType = 12		ifTrue: [^RsrOrderedCollectionReference new].	aReferenceType = 13		ifTrue: [^RsrDictionaryReference new].	aReferenceType = 14		ifTrue: [^RsrDateAndTimeReference new].	self error: 'ReferenceType(', aReferenceType printString, ') not yet implemented'.! !

!RsrDecoder methodsFor!
decodeReference: aStream	| oid |	oid := self decodeControlWord: aStream.	oid = self immediateOID ifTrue: [^self decodeImmediateReference: aStream].	^RsrServiceReference sid: oid! !

!RsrDecoder methodsFor!
decodeDeliverResponse: aStream    | transaction numServices serviceSnapshots response |    transaction := self decodeControlWord: aStream.    numServices := self decodeControlWord: aStream.    serviceSnapshots := (1 to: numServices) collect: [:each | self decodeServiceSnapshot: aStream].    response := self decodeReference: aStream.    ^RsrDeliverResponse new        transaction: transaction;        snapshots: serviceSnapshots;        response: response;        yourself! !

!RsrDecoder methodsFor!
decodeServiceSnapshot: aStream	| snapshot |	snapshot := RsrServiceSnapshot new.	snapshot		decode: aStream		using: self.	^snapshot! !

!RsrDecoder methodsFor!
decodeImmediateReference: aStream	| referenceType |	referenceType := self decodeControlWord: aStream.	^(self instanceOfImmediate: referenceType)		decode: aStream		using: self! !

!RsrDecoder methodsFor!
decodeSendMessage: aStream	| transaction argCount receiverReference selector numServices serviceSnapshots arguments instance |	transaction := self decodeControlWord: aStream.	numServices := self decodeControlWord: aStream.	serviceSnapshots := (1 to: numServices) collect: [:each | self decodeServiceSnapshot: aStream].	receiverReference := self decodeReference: aStream.	selector := self decodeReference: aStream.	argCount := self decodeControlWord: aStream.	arguments := (1 to: argCount) collect: [:each | self decodeReference: aStream].	instance := RsrSendMessage		transaction: transaction		receiver: receiverReference		selector: selector		arguments: arguments.	instance snapshots: serviceSnapshots.	^instance! !

!RsrDecoder methodsFor!
decodeCommand: aStream	"Decode an object from the stream"	| command |	command := self decodeControlWord: aStream.	command == self sendMessageCommand ifTrue: [^self decodeSendMessage: aStream].	command == self deliverResponseCommand ifTrue: [^self decodeDeliverResponse: aStream].	command == self releaseObjectsCommand ifTrue: [^self decodeReleaseServices: aStream].	command == self deliverErrorResponseCommand ifTrue: [^self decodeDeliverErrorResponse: aStream].	^RsrError signal: 'Unknown command identifier: ', command printString! !

!RsrDecoder methodsFor!
bytesAsInteger: bytes	| res |	res := 0.	bytes do: [:e | res := (res bitShift: 8) bitOr: e].	^res! !

!RsrDecoder methodsFor!
decodeDeliverErrorResponse: aStream	| transaction originalClassName tag messageText stack error |	transaction := self decodeControlWord: aStream.	originalClassName := (self decodeReference: aStream) resolve: nil.	tag := (self decodeReference: aStream) resolve: nil.	messageText := (self decodeReference: aStream) resolve: nil.	stack := (self decodeReference: aStream) resolve: nil.	error := RsrRemoteError new		originalClassName: originalClassName;		tag: tag;		messageText: messageText;		stack: stack;		yourself.	^RsrDeliverErrorResponse new		transaction: transaction;		remoteError: error;		yourself! !

!RsrDecoder methodsFor!
lookupClass: aClassName	^RsrClassResolver classNamed: aClassName! !

!RsrCycleDetected methodsFor!
messageText	^'Cycle detected on: ', object printString! !

!RsrCycleDetected methodsFor!
object: anObject	object := anObject! !

!RsrDeliverResponse methodsFor!
transaction: aTransactionId	transaction := aTransactionId! !

!RsrDeliverResponse methodsFor!
snapshots	^snapshots! !

!RsrDeliverResponse methodsFor!
response: anObject	response := anObject! !

!RsrDeliverResponse methodsFor!
executeFor: aConnection	| pendingMessage result |	pendingMessage := aConnection pendingMessages		removeKey: transaction		ifAbsent:			[^self error: 'Handle unknown transaction'].	[snapshots do: [:each | each reifyIn: aConnection].	result := response resolve: aConnection.	pendingMessage promise fulfill: result]		on: Error		do: [:ex | pendingMessage promise fulfill: ex copy]! !

!RsrDeliverResponse methodsFor!
reportOn: aLog	aLog debug: 'RsrDeliverResponse(', self response class name, ')'! !

!RsrDeliverResponse methodsFor!
transaction	^transaction! !

!RsrDeliverResponse methodsFor!
response	^response! !

!RsrDeliverResponse methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeDeliverResponse: self		onto: aStream! !

!RsrDeliverResponse methodsFor!
snapshots: anArrayOfSnapshots	snapshots := anArrayOfSnapshots! !

!RsrNullChannel methodsFor!
close	"NOP"! !

!RsrNullChannel methodsFor!
isOpen	^true! !

!RsrNullChannel methodsFor!
send: aCommand	"NOP"! !

!RsrNullChannel methodsFor!
open	"NOP"! !

!RsrNullChannel methodsFor!
received: aCommand	"NOP"! !

!RsrChannel methodsFor!
log	^self connection log! !

!RsrChannel methodsFor!
connection	^connection! !

!RsrChannel methodsFor!
close	"Ensure the channel is closed to further communication."	^self subclassResponsibility! !

!RsrChannel methodsFor!
isOpen	"Report whether the Channel is open between Connections."	^self subclassResponsibility! !

!RsrChannel methodsFor!
open	"Ensure the channel is open and ready for communication."	^self subclassResponsibility! !

!RsrChannel methodsFor!
send: aCommand	"Send the provided command over the channel."	^self subclassResponsibility! !

!RsrChannel methodsFor!
genericError: anError	^self connection unknownError: anError! !

!RsrChannel methodsFor!
received: aCommand	"A command has come in over the channel. Evaluate it."	self connection dispatchQueue dispatch: [aCommand executeFor: self connection]! !

!RsrChannel methodsFor!
connection: aConnection	connection := aConnection! !

!RsrBufferedSocketStream methodsFor!
next	^self next: 1! !

!RsrBufferedSocketStream methodsFor!
growOutBufferTo: aNumberOfBytes	| rounding |	rounding := ((aNumberOfBytes \\ 4096) + 1) * 4096.	outBuffer := outBuffer , (ByteArray new: rounding - outBuffer size)! !

!RsrBufferedSocketStream methodsFor!
close	stream close! !

!RsrBufferedSocketStream methodsFor!
flush	writePosition = nextToWrite		ifTrue: [^self].	stream nextPutAll: (outBuffer copyFrom: writePosition to: nextToWrite - 1).	writePosition := nextToWrite := 1.	stream flush! !

!RsrBufferedSocketStream methodsFor!
atEnd	^stream atEnd! !

!RsrBufferedSocketStream methodsFor!
initialize	super initialize.	outBuffer := ByteArray new: 4096.	nextToWrite := 1.	writePosition := 1! !

!RsrBufferedSocketStream methodsFor!
isConnected	^stream isConnected! !

!RsrBufferedSocketStream methodsFor!
nextPutAll: aByteArray	(outBuffer size >= (aByteArray size + nextToWrite))		ifFalse: [self growOutBufferTo: outBuffer size + nextToWrite].	outBuffer		replaceFrom: nextToWrite		to: nextToWrite + aByteArray size - 1		with: aByteArray		startingAt: 1.	nextToWrite := nextToWrite + aByteArray size.	self checkAutoFlush! !

!RsrBufferedSocketStream methodsFor!
stream: aStream	stream := aStream! !

!RsrBufferedSocketStream methodsFor!
next: aCount	^stream next: aCount! !

!RsrBufferedSocketStream methodsFor!
checkAutoFlush	nextToWrite > 4096		ifTrue: [ self flush ]! !

!RsrCommand methodsFor!
executeFor: aConnection	self subclassResponsibility! !

!RsrCommand methodsFor!
reportOn: aLog	self subclassResponsibility! !

!RsrCommand methodsFor!
encode: aStreamusing: anEncoder	self subclassResponsibility! !

!RsrInitiateConnection methodsFor!
connect	| socket channel connection |	socket := self socketClass new.	socket		connectToHost: self host		port: self port.	channel := RsrSocketChannel socket: socket.	connection := RsrConnection		channel: channel		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated		oidSpigot: RsrThreadSafeNumericSpigot naturals negated.	^connection open! !

!RsrThreadSafeNumericSpigot methodsFor!
initialize	super initialize.	mutex := Semaphore forMutualExclusion! !

!RsrThreadSafeNumericSpigot methodsFor!
next	^mutex critical: [super next]! !

!RsrPromise methodsFor!
value	self waitForFulfillment.	error isNil		ifFalse: [error copy signal].	^value! !

!RsrPromise methodsFor!
waitForFulfillment	self isFulfilled		ifTrue: [^self].	mutex wait.	mutex signal! !

!RsrPromise methodsFor!
error: anException	self isFulfilled		ifTrue: [^self error: 'Promise value already set'].	error := anException.	mutex signal	! !

!RsrPromise methodsFor!
initialize	super initialize.	value := markerValue := Object new.	mutex := Semaphore new! !

!RsrPromise methodsFor!
isFulfilled	^value ~~ markerValue! !

!RsrPromise methodsFor!
fulfill: anObject	self isFulfilled		ifTrue: [^self error: 'Promise value already set'].	value := anObject.	mutex signal! !

!RsrEncoder methodsFor!
encodeControlWord: anIntegeronto: aStream	| encodedInteger encodedBytes |	(anInteger between: self controlWordMin and: self controlWordMax)		ifFalse: [self error: anInteger printString, ' is outside the supported size of a control word.'].	encodedInteger := (anInteger positive		ifTrue: [anInteger]		ifFalse: [(2 raisedTo: 64) + anInteger]).	encodedBytes := self		integerAsByteArray: encodedInteger		ofSize: self sizeOfInteger.	aStream nextPutAll: encodedBytes! !

!RsrEncoder methodsFor!
encodeDeliverErrorResponse: aDeliverErrorResponse	^ByteArray streamContents: [:stream | self encodeDeliverErrorResponse: aDeliverErrorResponse onto: stream]! !

!RsrEncoder methodsFor!
encodeReference: aReferenceonto: aStream	aReference		encode: aStream		using: self! !

!RsrEncoder methodsFor!
encodeServiceSnapshot: aServiceSnapshot	^ByteArray		streamContents:			[:stream |			self				encodeServiceSnapshot: aServiceSnapshot				onto: stream]! !

!RsrEncoder methodsFor!
encodeServiceSnapshot: aServiceSnapshotonto: aStream	aServiceSnapshot		encode: aStream		using: self! !

!RsrEncoder methodsFor!
encodeDeliverResponse: aDeliverResponse	^ByteArray streamContents: [:stream | self encodeDeliverResponse: aDeliverResponse onto: stream]! !

!RsrEncoder methodsFor!
encodeDeliverResponse: aDeliverResponseonto: aStream	self		encodeControlWord: self deliverResponseCommand		onto: aStream.	self		encodeControlWord: aDeliverResponse transaction		onto: aStream.	self		encodeControlWord: aDeliverResponse snapshots size		onto: aStream.	aDeliverResponse snapshots do: [:each | self encodeServiceSnapshot: each onto: aStream].	self		encodeReference: aDeliverResponse response		onto: aStream! !

!RsrEncoder methodsFor!
encodeSendMessage: aSendMessageonto: aStream	self		encodeControlWord: self sendMessageCommand		onto: aStream.	self		encodeControlWord: aSendMessage transaction		onto: aStream.	self		encodeControlWord: aSendMessage snapshots size		onto: aStream.	aSendMessage snapshots		do:			[:each |			self				encodeServiceSnapshot: each				onto: aStream].	self		encodeReference:  aSendMessage receiver		onto: aStream.	self		encodeReference: aSendMessage selector		onto: aStream.	self		encodeControlWord: aSendMessage arguments size		onto: aStream.	aSendMessage arguments		do:			[:each |			self				encodeReference: each				onto: aStream]! !

!RsrEncoder methodsFor!
encodeSendMessage: aSendMessage	^ByteArray streamContents: [:stream | self encodeSendMessage: aSendMessage onto: stream]! !

!RsrEncoder methodsFor!
encodeDeliverErrorResponse: aDeliverErrorResponseonto: aStream	| error |	error := aDeliverErrorResponse remoteError.	self		encodeControlWord: self deliverErrorResponseCommand		onto: aStream.	self		encodeControlWord: aDeliverErrorResponse transaction		onto: aStream.	self		encodeReference: (RsrReference from: error originalClassName)		onto: aStream.	self		encodeReference: (RsrReference from: error tag)		onto: aStream.	self		encodeReference: (RsrReference from: error messageText)		onto: aStream.	self		encodeReference: (RsrReference from: error stack)		onto: aStream! !

!RsrEncoder methodsFor!
encodeReleaseServices: aReleaseServicesonto: aStream	self		encodeControlWord: self releaseObjectsCommand		onto: aStream.	self		encodeControlWord: aReleaseServices sids size		onto: aStream.	aReleaseServices sids		do:			[:sid |			self				encodeControlWord: sid				onto: aStream]! !

!RsrEncoder methodsFor!
encodeReleaseServices: aReleaseServices	^ByteArray streamContents: [:stream | self encodeReleaseServices: aReleaseServices onto: stream]! !

!RsrEncoder methodsFor!
integerAsByteArray: anIntegerofSize: aNumberOfBytes	| bytes int |	bytes := ByteArray new: aNumberOfBytes.	int := anInteger.	aNumberOfBytes		to: 1		by: -1		do:			[:i | | byte |			byte := int bitAnd: 16rFF.			int := int bitShift: -8.			bytes at: i put: byte].	int ~= 0		ifTrue: [self error: 'Loss of precision detected'].	^bytes! !

!RsrService methodsFor!
isClient	^self class isClientClass! !

!RsrService methodsFor!
reflectedVariableNames	^RsrServiceSnapshot reflectedVariablesFor: self! !

!RsrService methodsFor!
registerWith: aConnection	aConnection ensureRegistered: self! !

!RsrService methodsFor!
_id	^_id! !

!RsrService methodsFor!
postUpdate	"#postUpdate is called just after the Service's shared variables are updated by the framework.	This method can be overridden to ensure internal consistency."	^self! !

!RsrService methodsFor!
_id: anRsrIdconnection: aConnection	_id := anRsrId.	_connection := aConnection.	remoteSelf := aConnection _forwarderClass on: self! !

!RsrService methodsFor!
isMirrored	^_connection ~~ nil! !

!RsrService methodsFor!
_synchronize	"Return self to synchronize with the remote peer"	^self! !

!RsrService methodsFor!
preUpdate	"#preUpdate is called just before the Service's shared variables are updated by the framework.	This method can be overridden to ensure internal consistency.	Note: If this method raises an exception, RSR will not signal #postUpdate."	^self! !

!RsrService methodsFor!
isServer	^self class isServerClass! !

!RsrService methodsFor!
_connection	^_connection! !

!RsrService methodsFor!
synchronize	"Synchronize the service w/ its peer."	remoteSelf == nil		ifFalse: [remoteSelf _synchronize]! !

!RsrService methodsFor!
isNotMirrored	^self isMirrored not! !

!RsrSnapshotAnalysis methodsFor!
roots: anArray	roots := anArray! !

!RsrSnapshotAnalysis methodsFor!
snapshot: aService	snapshots add: (RsrServiceSnapshot from: aService)! !

!RsrSnapshotAnalysis methodsFor!
snapshots	^snapshots! !

!RsrSnapshotAnalysis methodsFor!
analyzeDictionary: aDictionary	self		analyzing: aDictionary		during:			[aDictionary				keysAndValuesDo:					[:key :value |					self						analyze: key;						analyze: value]].	^aDictionary! !

!RsrSnapshotAnalysis methodsFor!
analyzeImmediate: anImmediateObject	^anImmediateObject! !

!RsrSnapshotAnalysis methodsFor!
analyze: anObject	^(self referenceClassFor: anObject)		analyze: anObject		using: self! !

!RsrSnapshotAnalysis methodsFor!
ensureRegistered: aService	self connection ensureRegistered: aService! !

!RsrSnapshotAnalysis methodsFor!
snapshots: anOrderedCollection	snapshots := anOrderedCollection! !

!RsrSnapshotAnalysis methodsFor!
initialize	super initialize.	snapshots := OrderedCollection new.	inFlight := IdentitySet new! !

!RsrSnapshotAnalysis methodsFor!
analyzing: anObjectduring: aBlock	(inFlight includes: anObject)		ifTrue: [^RsrCycleDetected signal: anObject].	inFlight add: anObject.	aBlock value.	inFlight remove: anObject! !

!RsrSnapshotAnalysis methodsFor!
analyzeCollection: aCollection	self		analyzing: aCollection		during: [aCollection do: [:each | self analyze: each]].	^aCollection! !

!RsrSnapshotAnalysis methodsFor!
connection: aConnection	connection := aConnection! !

!RsrSnapshotAnalysis methodsFor!
roots	^roots! !

!RsrSnapshotAnalysis methodsFor!
analyzeService: aService	self ensureRegistered: aService.	self		analyzing: aService		during:			[RsrServiceSnapshot				reflectedVariablesFor: aService				do: [:each | self analyze: each]].	snapshots add: (RsrServiceSnapshot from: aService)! !

!RsrSnapshotAnalysis methodsFor!
referenceClassFor: anObject	^RsrReference referenceClassFor: anObject! !

!RsrSnapshotAnalysis methodsFor!
connection	^connection! !

!RsrSnapshotAnalysis methodsFor!
perform	roots do: [:each | self analyze: each]! !