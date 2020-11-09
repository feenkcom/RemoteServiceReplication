| package |
package := Package name: 'RemoteServiceReplication'.
package paxVersion: 1; basicComment: ''.

package classNames
	add: #RsrLog;
	add: #RsrReleaseServices;
	add: #RsrRemoteExceptionClient;
	add: #RsrRemotePromiseResolver;
	add: #RsrStream;
	add: #RsrInMemoryChannel;
	add: #RsrLogWithPrefix;
	add: #RsrAcceptConnection;
	add: #RsrEncoder;
	add: #RsrPendingMessage;
	add: #RsrCommandSink;
	add: #RsrServiceFactoryServer;
	add: #RsrLogSink;
	add: #RsrSendMessage;
	add: #RsrServiceSnapshot;
	add: #RsrThreadSafeDictionary;
	add: #RsrRemoteExceptionServer;
	add: #RsrSocketChannel;
	add: #RsrMessageSend;
	add: #RsrService;
	add: #RsrInitiateConnection;
	add: #RsrAbstractReason;
	add: #RsrCommandSource;
	add: #RsrCommand;
	add: #RsrPromise;
	add: #RsrBufferedSocketStream;
	add: #RsrCustomSink;
	add: #RsrConnection;
	add: #RsrCycleDetected;
	add: #RsrSnapshotAnalysis;
	add: #RsrCodec;
	add: #RsrNumericSpigot;
	add: #RsrServiceFactory;
	add: #RsrDispatchQueue;
	add: #RsrDeliverResponse;
	add: #RsrPromiseResolutionAction;
	add: #RsrRemoteException;
	add: #RsrSocketStream;
	add: #RsrChannel;
	add: #RsrTranscriptSink;
	add: #RsrConnectionSpecification;
	add: #RsrRemoteError;
	add: #RsrSocketChannelLoop;
	add: #RsrDecoder;
	add: #RsrThreadSafeNumericSpigot;
	add: #RsrServiceFactoryClient;
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
	subclass: #RsrMessageSend
	instanceVariableNames: 'receiver selector arguments'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrMessageSend categoriesForClass!RemoteServiceReplication! !

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
	instanceVariableNames: 'mutex value state resolvedMutex resolutionActions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrPromise comment: 'Purpose: Provide a simple Promise interface for use in RSR.A Promise may be in two high level states -- unresolved and resolved. Resolving a promise means either breaking or fulfilling the promise. Any users of the Notification Interface will be notified of the resolution. See individual methods for details.Resolution Interface:- #break:- #fulfill:Notification Interface:- #when:catch:- #wait- #valueExample Usage:```	| promise |	promise := Promise new.	promise		when: [:anObject | Transcript show: ''Promise fulfilled to: '', anObject asString; cr]		catch: [:reason | Transcript show: ''Promise broken because of '', reason asString; cr].	"Time passes"	promise fulfill: Object new``````	| promise |	promise := Promise new.	self runAsynCalculationThenFulfill: promise.	promise wait.```'!
!RsrPromise categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrPromiseResolutionAction
	instanceVariableNames: 'when catch'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrPromiseResolutionAction categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrRemotePromiseResolver
	instanceVariableNames: 'mutex sendMessage connection extraRoots hasResolved'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrRemotePromiseResolver comment: 'This class is responsible for taking breaking or fulfilling its associated Promise. The Promise exists in the remote RSR instance.This class may be mutated outside of the thread which created it. Therefore, it contains a protection mutex to ensure consistency.'!
!RsrRemotePromiseResolver categoriesForClass!RemoteServiceReplication! !

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

RsrObject
	subclass: #RsrThreadSafeDictionary
	instanceVariableNames: 'mutex map'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrThreadSafeDictionary comment: 'I maintain the associations between locally stored objects and their remote counterparts.'!
!RsrThreadSafeDictionary categoriesForClass!RemoteServiceReplication! !

RsrService
	subclass: #RsrAbstractReason
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrAbstractReason comment: 'This Service services as an abstract superclass for various Promise break reasons generated via the framework.'!
!RsrAbstractReason categoriesForClass!RemoteServiceReplication! !

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
	subclass: #RsrDeliverResponse
	instanceVariableNames: 'transaction responseReference snapshots'
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
	instanceVariableNames: 'transaction receiverReference selectorReference argumentReferences snapshots'
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

RsrAbstractReason
	subclass: #RsrRemoteException
	instanceVariableNames: 'exceptionClassName tag messageText stack'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRemoteException categoriesForClass!RemoteServiceReplication! !

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

RsrRemoteException
	subclass: #RsrRemoteExceptionClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRemoteExceptionClient categoriesForClass!RemoteServiceReplication! !

RsrRemoteException
	subclass: #RsrRemoteExceptionServer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRemoteExceptionServer categoriesForClass!RemoteServiceReplication! !

!RsrReference class methodsFor!
referenceClassFor: anObject	(anObject isKindOf: RsrService)		ifTrue: [^RsrServiceReference].	^self referenceMapping		at: anObject class		ifAbsent: [RsrUnsupportedObject signal: anObject]! !

!RsrForwarder class methodsFor!
on: anRsrObject	| instance |	instance := self new.	instance _service: anRsrObject.	^instance! !

!RsrForwarder methodsFor!
doesNotUnderstand: aMessage	^_service _connection		_sendMessage: aMessage		to: _service! !

!RsrForwarder methodsFor!
_service: aService	_service := aService! !

!RsrNumericSpigot class methodsFor!
new	^self		start: 0		step: 1! !

!RsrNumericSpigot class methodsFor!
naturals	^self		start: 1		step: 1! !

!RsrNumericSpigot class methodsFor!
start: aNumberstep: anIncrement	^super new		start: aNumber;		step: anIncrement;		yourself! !

!RsrRemoteError class methodsFor!
from: anException	| tag |	tag := anException tag		ifNotNil:			[[anException tag asString]				on: Error				do: [:ex | ex return: 'Unable to pack #tag containing an instance of ', anException tag class name]].	^self new		originalClassName: anException class name;		tag: tag;		messageText: anException messageText;		stack: RsrProcessModel currentStackDump;		yourself! !

!RsrDeliverResponse class methodsFor!
transaction: aTransactionIdresponseReference: aReferencesnapshots: anArrayOfSnapshots	^self new		transaction: aTransactionId;		responseReference: aReference;		snapshots: anArrayOfSnapshots;		yourself! !

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
transaction: aTransactionIdreceiverReference: aServiceReferenceselectorReference: aSelectorReferenceargumentReferences: anArrayOfReferences	^self new		transaction: aTransactionId;		receiverReference: aServiceReference;		selectorReference: aSelectorReference;		argumentReferences: anArrayOfReferences;		yourself! !

!RsrPromiseResolutionAction class methodsFor!
when: aWhenBlockcatch: aCatchBlock	^self new		when: aWhenBlock;		catch: aCatchBlock;		yourself! !

!RsrRemotePromiseResolver class methodsFor!
for: aSendMessageover: aConnection	^self new		sendMessage: aSendMessage;		connection: aConnection;		yourself! !

!RsrBufferedSocketStream class methodsFor!
on: aSocketStream	^self new		stream: aSocketStream;		yourself! !

!RsrInMemoryChannel class methodsFor!
inQueue: inQueueoutQueue: outQueue	^self new		inQueue: inQueue;		outQueue: outQueue;		yourself! !

!RsrAcceptConnection class methodsFor!
wildcardAddress	^'0.0.0.0'! !

!RsrAcceptConnection class methodsFor!
port: aPortInteger	^self		host: self wildcardAddress		port: aPortInteger! !

!RsrCustomSink class methodsFor!
action: aBlock	^self new		action: aBlock;		yourself! !

!RsrMessageSend class methodsFor!
receiver: anObjectselector: aSelectorarguments: anArray	^self new		receiver: anObject;		selector: aSelector;		arguments: anArray;		yourself! !

!RsrLogWithPrefix class methodsFor!
prefix: aStringlog: aLog	^self new		prefix: aString;		log: aLog;		yourself! !

!RsrLogWithPrefix class methodsFor!
log: aLog	^self new		log: aLog;		yourself! !

!RsrReleaseServices class methodsFor!
sids: anArrayOfServiceIDs	^self new		sids: anArrayOfServiceIDs;		yourself! !

!RsrPendingMessage class methodsFor!
services: aListpromise: aPromise	^self new		services: aList;		promise: aPromise;		yourself! !

!RsrStream class methodsFor!
on: aStream	^self new		stream: aStream;		yourself! !

!RsrCycleDetected class methodsFor!
signal: anObject	^self new		object: anObject;		signal! !

!RsrRemoteException class methodsFor!
clientClassName	^#RsrRemoteExceptionClient! !

!RsrRemoteException class methodsFor!
serverClassName	^#RsrRemoteExceptionServer! !

!RsrRemoteException class methodsFor!
templateClassName	^#RsrRemoteException! !

!RsrRemoteException class methodsFor!
from: anException	"Create an instance of the RemoteException reason.	The client is used here because once we send it, we are done with it.	The client will GC and the server will later GC. We don't care to have	a server hanging around if we don't need it."	| tag |	tag := anException tag		ifNotNil:			[[anException tag asString]				on: Error				do: [:ex | ex return: 'Unable to pack #tag containing an instance of ', anException tag class name]].	^self clientClass new		exceptionClassName: anException class name;		tag: tag;		messageText: anException messageText;		stack: RsrProcessModel currentStackDump;		yourself! !

!RsrConnection class methodsFor!
new	"Instances of Connection should not be created via #new.	Instead use ConnectionSpecification.	See SystemTestCase>>#setUp for an example."	self shouldNotImplement: #new! !

!RsrConnection class methodsFor!
channel: aChanneltransactionSpigot: aNumericSpigotoidSpigot: anOidSpigot	^super new		channel: aChannel;		transactionSpigot: aNumericSpigot;		oidSpigot: anOidSpigot;		yourself! !

!RsrSnapshotAnalysis class methodsFor!
roots: anArrayconnection: aConnection	^self new		roots: anArray;		connection: aConnection;		yourself! !

!RsrDecoder class methodsFor!
registry: aRegistry	^self new		registry: aRegistry;		yourself! !

!RsrConnectionSpecification class methodsFor!
host: hostnameOrAddressport: port	^self new		host: hostnameOrAddress;		port: port;		yourself! !

!RsrSocketStream class methodsFor!
on: anRsrSocket	^self new		socket: anRsrSocket;		yourself! !

!RsrSocketChannel class methodsFor!
socket: aSocket	^self new		socket: aSocket;		yourself! !

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

!RsrPromiseResolutionAction methodsFor!
catch	^catch! !

!RsrPromiseResolutionAction methodsFor!
when: aBlock	when := aBlock! !

!RsrPromiseResolutionAction methodsFor!
catch: aBlock	catch := aBlock! !

!RsrPromiseResolutionAction methodsFor!
when	^when! !

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

!RsrMessageSend methodsFor!
selector: aSelector	selector := aSelector! !

!RsrMessageSend methodsFor!
arguments: anArray	arguments := anArray! !

!RsrMessageSend methodsFor!
receiver	^receiver! !

!RsrMessageSend methodsFor!
receiver: anObject	receiver := anObject! !

!RsrMessageSend methodsFor!
arguments	^arguments! !

!RsrMessageSend methodsFor!
selector	^selector! !

!RsrMessageSend methodsFor!
perform	^self receiver		perform: self selector		withArguments: self arguments! !

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

!RsrRemotePromiseResolver methodsFor!
sendMessage	^sendMessage! !

!RsrRemotePromiseResolver methodsFor!
fulfill: result	"Fulfill the remote promise with a fulfilled value of <result>"	self resolution: (Array with: #fulfill with: result)! !

!RsrRemotePromiseResolver methodsFor!
sendResultReference: resultReferencesnapshots: snapshots	| response |	response := RsrDeliverResponse				transaction: self sendMessage transaction				responseReference: resultReference				snapshots: snapshots.	self connection _sendCommand: response! !

!RsrRemotePromiseResolver methodsFor!
hasResolved	^hasResolved! !

!RsrRemotePromiseResolver methodsFor!
initialize	super initialize.	extraRoots := OrderedCollection new.	hasResolved := false.	mutex := Semaphore forMutualExclusion! !

!RsrRemotePromiseResolver methodsFor!
connection: aConnection	connection := aConnection! !

!RsrRemotePromiseResolver methodsFor!
resolution: result	"Process and dispatch the result"	mutex		critical:			[self hasResolved ifTrue: [^self].			[self				sendResult: result				closureRoots: (Array with: result), extraRoots]				on: self sendMessage unhandledExceptionClass				do:					[:ex | | answer |					answer := Array						with: #break						with: (RsrRemoteException from: ex).					self						sendResult: answer						closureRoots: answer.					ex return].			hasResolved := true]! !

!RsrRemotePromiseResolver methodsFor!
addRoot: aService	mutex critical: [extraRoots add: aService]! !

!RsrRemotePromiseResolver methodsFor!
break: aReason	"<aReason> can be any object supported by RSR."	self resolution: (Array with: #break with: aReason)! !

!RsrRemotePromiseResolver methodsFor!
sendMessage: aSendMessage	sendMessage := aSendMessage! !

!RsrRemotePromiseResolver methodsFor!
sendResult: resultclosureRoots: roots	| analysis resultReference |	analysis := RsrSnapshotAnalysis		roots: roots		connection: self connection.	analysis perform.	resultReference := RsrReference from: result.	self		sendResultReference: resultReference		snapshots: analysis snapshots! !

!RsrRemotePromiseResolver methodsFor!
connection	^connection! !

!RsrRemotePromiseResolver methodsFor!
assertNotResolved	self hasResolved		ifTrue: [RsrAlreadyResolved signal]! !

!RsrInMemoryChannel methodsFor!
drainLoop	| command |	[command := inQueue next.	command isNil]		whileFalse:			[self received: command].	self connection channelDisconnected! !

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

!RsrLogSink methodsFor!
write: aMessage	self subclassResponsibility! !

!RsrCommandSource methodsFor!
decoder	^RsrDecoder new! !

!RsrCommandSource methodsFor!
nextCommand	^self decoder decodeCommand: self stream! !

!RsrCommandSource methodsFor!
executeCycle	[| command |	command := self nextCommand.	self report: command.	self channel received: command]		on: RsrSocketClosed		do:			[:ex |			self reportException: ex.			self channel channelDisconnected]! !

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
selectorReference: aSymbolReference	selectorReference := aSymbolReference! !

!RsrSendMessage methodsFor!
transaction: anObject	transaction := anObject! !

!RsrSendMessage methodsFor!
snapshots	^snapshots! !

!RsrSendMessage methodsFor!
snapshots: anArrayOfSnapshots	snapshots := anArrayOfSnapshots! !

!RsrSendMessage methodsFor!
logException: anExceptionto: aLog	| message |	message := String		streamContents:			[:stream |			stream				print: self receiverReference;				nextPutAll: '>>';				print: self selectorReference;				nextPutAll: ' due to: ';				nextPutAll: anException description].	aLog error: message! !

!RsrSendMessage methodsFor!
executeFor: aConnection	| resolver services receiver selector arguments messageSend |	resolver := RsrRemotePromiseResolver		for: self		over: aConnection.	[services := self snapshots collect: [:each | each reifyIn: aConnection].	receiver := self receiverReference resolve: aConnection.	selector := self selectorReference resolve: aConnection.	arguments := self argumentReferences collect: [:each | each resolve: aConnection]]		on: Error		do:			[:ex |			self				logException: ex				to: aConnection log.			self note: 'This code path needs to be tested'.			^resolver break: (RsrRemoteException from: ex)].	resolver addRoot: receiver. "Ensure we always send back the receiver -- this ensures sending a message results in by-directional syncing."	messageSend := RsrMessageSend		receiver: receiver		selector: selector		arguments: arguments.	self		perform: messageSend		answerUsing: resolver! !

!RsrSendMessage methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeSendMessage: self		onto: aStream! !

!RsrSendMessage methodsFor!
selectorReference	^selectorReference! !

!RsrSendMessage methodsFor!
transaction	^ transaction! !

!RsrSendMessage methodsFor!
perform: aMessageSendanswerUsing: aResolver	[| raisedException |	raisedException := false.	[aResolver fulfill: aMessageSend perform]		on: self unhandledExceptionClass		do:			[:ex | | debugResult |			debugResult := [aMessageSend receiver									debug: ex									raisedDuring: aMessageSend									answerUsing: aResolver]									on: self unhandledExceptionClass									do:										[:debugEx | 										raisedException := true.										aResolver break: (RsrRemoteException from: debugEx).										ex return].			aResolver hasResolved				ifTrue: [ex return]				ifFalse:					[ex isResumable						ifTrue: [ex resume: debugResult]						ifFalse:							[aResolver break: (RsrRemoteException from: ex).							ex return]]]]		ensure:			[aResolver hasResolved				ifFalse: [aResolver break: 'Message send terminated without a result']]! !

!RsrSendMessage methodsFor!
unhandledExceptionClass	"Temporarily, use Error until we have appropriate GemStone hooks."	^Error! !

!RsrSendMessage methodsFor!
reportOn: aLog	aLog debug: 'RsrSendMessage(', self receiverReference asString, '>>', self selectorReference asString, ')'! !

!RsrSendMessage methodsFor!
argumentReferences	^argumentReferences! !

!RsrSendMessage methodsFor!
argumentReferences: anArrayOfReferences	argumentReferences := anArrayOfReferences! !

!RsrSendMessage methodsFor!
receiverReference	^receiverReference! !

!RsrSendMessage methodsFor!
receiverReference: aServiceReference	receiverReference := aServiceReference! !

!RsrThreadSafeDictionary methodsFor!
removeKey: anRsrIdifAbsent: aBlock	| element wasRemoved |	wasRemoved := true.	element := mutex critical: [map removeKey: anRsrId ifAbsent: [wasRemoved := false]].	^wasRemoved		ifTrue: [element]		ifFalse: [aBlock value]! !

!RsrThreadSafeDictionary methodsFor!
do: aBlock	| values |	values := mutex critical: [map values].	values do: aBlock! !

!RsrThreadSafeDictionary methodsFor!
at: aKeyifAbsent: aBlock	| isPresent result |	isPresent := true.	result := mutex critical: [map at: aKey ifAbsent: [isPresent := false]].	^isPresent		ifTrue: [result]		ifFalse: [aBlock value]! !

!RsrThreadSafeDictionary methodsFor!
initialize	super initialize.	mutex := Semaphore forMutualExclusion.	map := Dictionary new! !

!RsrThreadSafeDictionary methodsFor!
removeKey: anRsrId	^mutex critical: [map removeKey: anRsrId ifAbsent: [nil]]! !

!RsrThreadSafeDictionary methodsFor!
at: aKeyput: aValue	mutex critical: [map at: aKey put: aValue].	^aValue! !

!RsrServiceSnapshot methodsFor!
snapshot: aService	sid := aService _id.	template := aService class templateClassName.	targetServiceType := aService isClient		ifTrue: [#server]		ifFalse: [#client].	slots := OrderedCollection new.	RsrServiceSnapshot		reflectedVariablesFor: aService		do: [:each | slots add: (RsrReference from: each)]! !

!RsrServiceSnapshot methodsFor!
slots: anArrayOfReferences	slots := anArrayOfReferences! !

!RsrServiceSnapshot methodsFor!
targetServiceType: aSymbol	targetServiceType := aSymbol! !

!RsrServiceSnapshot methodsFor!
template: aSymbol	template := aSymbol! !

!RsrServiceSnapshot methodsFor!
instanceIn: aConnection	| instance |	instance := aConnection		serviceAt: self sid		ifAbsent: [self createInstanceRegisteredIn: aConnection].	self shouldCreateServer		ifTrue: [aConnection _stronglyRetain: instance].	^instance! !

!RsrServiceSnapshot methodsFor!
slots	^slots! !

!RsrServiceSnapshot methodsFor!
createInstanceRegisteredIn: aConnection	| instance |	instance := self shouldCreateServer		ifTrue: [self templateClass serverClass basicNew]		ifFalse: [self templateClass clientClass basicNew].	aConnection		_register: instance		as: self sid.	^instance! !

!RsrServiceSnapshot methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeControlWord: self snapshotIdentifier		onto: aStream.	anEncoder		encodeControlWord: self sid		onto: aStream.	anEncoder		encodeControlWord: self slots size		onto: aStream.	self targetClassNameReference		encode: aStream		using: anEncoder.	self slots do: [:each | each encode: aStream using: anEncoder]! !

!RsrServiceSnapshot methodsFor!
decode: aStreamusing: aDecoder	| species instVarCount serviceName templateClass |	species := aDecoder decodeControlWord: aStream.	sid := aDecoder decodeControlWord: aStream.	instVarCount := aDecoder decodeControlWord: aStream.	serviceName := (aDecoder decodeReference: aStream) resolve: nil.	templateClass := (RsrClassResolver classNamed: serviceName) templateClass.	template := templateClass templateClassName.	targetServiceType := templateClass clientClassName = serviceName		ifTrue: [#client]		ifFalse: [#server].	slots := OrderedCollection new: instVarCount.	instVarCount timesRepeat: [slots add: (aDecoder decodeReference: aStream)]! !

!RsrServiceSnapshot methodsFor!
shouldCreateServer	^self targetServiceType == #server! !

!RsrServiceSnapshot methodsFor!
sid	^sid! !

!RsrServiceSnapshot methodsFor!
reifyIn: aConnection	| instance referenceStream |	instance := self instanceIn: aConnection.	(self class reflectedVariablesFor: instance) size = slots size		ifFalse: [self error: 'Incorrected encoded instance detected'].	referenceStream := ReadStream on: slots.	instance preUpdate.	self class		reflectedVariableIndicesFor: instance		do: [:index | instance instVarAt: index put: (referenceStream next resolve: aConnection)].	instance postUpdate.	^instance! !

!RsrServiceSnapshot methodsFor!
snapshotIdentifier	^0! !

!RsrServiceSnapshot methodsFor!
sid: aServiceID	sid := aServiceID! !

!RsrServiceSnapshot methodsFor!
targetClassNameReference	| targetClassName |	targetClassName := self shouldCreateServer		ifTrue: [self templateClass serverClassName]		ifFalse: [self templateClass clientClassName].	^RsrSymbolReference symbol: targetClassName! !

!RsrServiceSnapshot methodsFor!
targetServiceType	^targetServiceType! !

!RsrServiceSnapshot methodsFor!
template	^template! !

!RsrServiceSnapshot methodsFor!
templateClass	^RsrClassResolver classNamed: self template! !

!RsrAcceptConnection methodsFor!
waitForConnection	| socket channel connection |	listener := self socketClass new.	[listener		bindAddress: self host		port: self port.	listener listen: 1.	socket := [listener accept]		on: RsrSocketError		do: [:ex | ex resignalAs: RsrWaitForConnectionCancelled new]]			ensure:				[listener close.				listener := nil].	channel := RsrSocketChannel socket: socket.	connection := RsrConnection		channel: channel		transactionSpigot: RsrThreadSafeNumericSpigot naturals		oidSpigot: RsrThreadSafeNumericSpigot naturals.	^connection open! !

!RsrAcceptConnection methodsFor!
cancelWaitForConnection	listener ifNotNil: [:socket | socket close]! !

!RsrAcceptConnection methodsFor!
isWaitingForConnection	^listener ~~ nil! !

!RsrDispatchQueue methodsFor!
async: aBlock	"Evaluate the block asynchronously and do not return a result"	queue nextPut: aBlock.	^nil! !

!RsrDispatchQueue methodsFor!
initialize	super initialize.	queue := SharedQueue new! !

!RsrDispatchQueue methodsFor!
runLoop	[self isRunning]		whileTrue:			[[queue next value]				on: Error				do: [:ex | self trace. Transcript show: ex messageText; cr. ex pass]]! !

!RsrDispatchQueue methodsFor!
dispatch: aBlock	^self async: aBlock! !

!RsrDispatchQueue methodsFor!
stop	"Stop process events in the dispatch queue."	isRunning := false.	self dispatch: []. "Ensure another action is added to the queue to ensure shutdown if it hasn't yet happened."	process := nil! !

!RsrDispatchQueue methodsFor!
start	"Start processing queued events."	isRunning := true.	process := RsrProcessModel fork: [self runLoop]! !

!RsrDispatchQueue methodsFor!
isRunning	^isRunning! !

!RsrConnection methodsFor!
serviceAt: aSIDifAbsent: aBlock	"Return the service associated with the provided SID."	| entry |	entry := registry at: aSID ifAbsent: [nil].	"Ensure we do not hold the lock for long."	entry == nil		ifTrue: [^aBlock value].	"The Service may have been garbage collected but	the entry may not yet be removed. Ensure we	evaluate the block in that case as well."	^entry service		ifNil: aBlock		ifNotNil: [:service | service]! !

!RsrConnection methodsFor!
serviceFor: aResponsibility	^self serviceFactory serviceFor: aResponsibility! !

!RsrConnection methodsFor!
close	channel close.	dispatchQueue stop.	pendingMessages do: [:each | each promise error: RsrConnectionClosed new].	"The following assignments probably shouldn't exist in this form.	Close needs to be idempotent."	pendingMessages := Dictionary new.	registry := RsrThreadSafeDictionary new.	closeSemaphore signal! !

!RsrConnection methodsFor!
log	^log! !

!RsrConnection methodsFor!
_remoteClientReleased: aSID	"Remotely, a Client instance has been garbage collected.	Ensure we only reference the associated service weakly."	| entry |	entry := registry		at: aSID		ifAbsent: [^self].	entry becomeWeak.! !

!RsrConnection methodsFor!
initialize	super initialize.	transactionSpigot := RsrThreadSafeNumericSpigot naturals.	pendingMessages := RsrThreadSafeDictionary new.	registry := RsrThreadSafeDictionary new.	dispatchQueue := RsrDispatchQueue new.	log := RsrLog new.	closeSemaphore := Semaphore new.! !

!RsrConnection methodsFor!
mournActionForServerSID: aSID	^[dispatchQueue dispatch: [registry removeKey: aSID]]! !

!RsrConnection methodsFor!
channel: aChannel	channel := aChannel.	channel connection: self! !

!RsrConnection methodsFor!
oidSpigot	^oidSpigot! !

!RsrConnection methodsFor!
_register: aServiceas: sid	| registryEntry mournAction |	aService		_id: sid		connection: self.	mournAction := aService isClient		ifTrue: [self mournActionForClientSID: sid]		ifFalse: [self mournActionForServerSID: sid].	registryEntry := RsrRegistryEntry		service: aService		onMourn: mournAction.	registry		at: sid		put: registryEntry! !

!RsrConnection methodsFor!
_ensureRegistered: aService	aService _connection == nil		ifTrue: [^self _register: aService as: oidSpigot next].	aService _connection == self		ifFalse: [^RsrAlreadyRegistered signalService: aService intendedConnection: self]! !

!RsrConnection methodsFor!
oidSpigot: anIntegerSpigot	oidSpigot := anIntegerSpigot! !

!RsrConnection methodsFor!
transactionSpigot	^transactionSpigot! !

!RsrConnection methodsFor!
serviceAt: aSID	^self		serviceAt: aSID		ifAbsent: [RsrUnknownSID signal: aSID printString]! !

!RsrConnection methodsFor!
_sendMessage: aMessageto: aService"Open coordination window"	"Send dirty transitive closure of aRemoteMessage"	"Send DispatchMessage command""Coorination window closed"	"Return Promise"	| analysis receiverReference selectorReference argumentReferences dispatchCommand promise pendingMessage |	self isOpen		ifFalse: [self error: 'Connection is not open'].	analysis := RsrSnapshotAnalysis		roots: (Array with: aService), aMessage arguments		connection: self.	analysis perform.	receiverReference := RsrReference from: aService.	selectorReference := RsrReference from: aMessage selector.	argumentReferences := aMessage arguments collect: [:each | RsrReference from: each].	dispatchCommand := RsrSendMessage		transaction: self transactionSpigot next		receiverReference: receiverReference		selectorReference: selectorReference		argumentReferences: argumentReferences.	dispatchCommand snapshots: analysis snapshots.	promise := RsrPromise new.	pendingMessage := RsrPendingMessage		services: nil "I don't think we need to cache services here. They will remain on the stack unless they were removed from the transitive closure by another proc"		promise: promise.	self pendingMessages		at: dispatchCommand transaction		put: pendingMessage.	self _sendCommand: dispatchCommand.	^promise! !

!RsrConnection methodsFor!
pendingMessages	^pendingMessages! !

!RsrConnection methodsFor!
_receivedCommand: aCommand	"Execute the command in the context of the receiving Connection."	RsrProcessModel fork: [aCommand executeFor: self]! !

!RsrConnection methodsFor!
unknownError: anException	self close! !

!RsrConnection methodsFor!
initializeServiceFactory	| instance |	instance := RsrServiceFactory clientClass new.	self _ensureRegistered: instance.	serviceFactory := instance.	^serviceFactory! !

!RsrConnection methodsFor!
_sendCommand: aCommand	"Send the provided Command to our peer."	channel send: aCommand! !

!RsrConnection methodsFor!
mournActionForClientSID: aSID	^[dispatchQueue		dispatch:			[registry removeKey: aSID.			self releaseOid: aSID]]! !

!RsrConnection methodsFor!
waitUntilClose	closeSemaphore		wait;		signal! !

!RsrConnection methodsFor!
channelDisconnected	self log info: 'Disconnected'.	self close! !

!RsrConnection methodsFor!
releaseOid: anOid	| command |	self isOpen		ifFalse: [^self].	self log trace: 'Cleaning up OID:', anOid printString.	command := RsrReleaseServices sids: (Array with: anOid).	self _sendCommand: command! !

!RsrConnection methodsFor!
_forwarderClass	^RsrForwarder! !

!RsrConnection methodsFor!
serviceFactory	^serviceFactory ifNil: [self initializeServiceFactory]! !

!RsrConnection methodsFor!
open	dispatchQueue start.	channel open! !

!RsrConnection methodsFor!
transactionSpigot: anObject	transactionSpigot := anObject! !

!RsrConnection methodsFor!
_stronglyRetain: aServer	"Retain the already registered server strongly."	| entry |	entry := registry		at: aServer _id		ifAbsent: [RsrUnknownSID signal: aServer _id printString].	entry becomeStrong! !

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
decodeSendMessage: aStream	| transaction argCount receiverReference selector numServices serviceSnapshots arguments instance |	transaction := self decodeControlWord: aStream.	numServices := self decodeControlWord: aStream.	serviceSnapshots := (1 to: numServices) collect: [:each | self decodeServiceSnapshot: aStream].	receiverReference := self decodeReference: aStream.	selector := self decodeReference: aStream.	argCount := self decodeControlWord: aStream.	arguments := (1 to: argCount) collect: [:each | self decodeReference: aStream].	instance := RsrSendMessage		transaction: transaction		receiverReference: receiverReference		selectorReference: selector		argumentReferences: arguments.	instance snapshots: serviceSnapshots.	^instance! !

!RsrDecoder methodsFor!
decodeImmediateReference: aStream	| referenceType |	referenceType := self decodeControlWord: aStream.	^(self instanceOfImmediate: referenceType)		decode: aStream		using: self! !

!RsrDecoder methodsFor!
decodeCommand: aStream	"Decode an object from the stream"	| command |	command := self decodeControlWord: aStream.	command == self sendMessageCommand ifTrue: [^self decodeSendMessage: aStream].	command == self deliverResponseCommand ifTrue: [^self decodeDeliverResponse: aStream].	command == self releaseObjectsCommand ifTrue: [^self decodeReleaseServices: aStream].	^RsrError signal: 'Unknown command identifier: ', command printString! !

!RsrDecoder methodsFor!
bytesAsInteger: bytes	| res |	res := 0.	bytes do: [:e | res := (res bitShift: 8) bitOr: e].	^res! !

!RsrDecoder methodsFor!
lookupClass: aClassName	^RsrClassResolver classNamed: aClassName! !

!RsrRemoteError methodsFor!
originalClassName	^originalClassName! !

!RsrRemoteError methodsFor!
stack: aString	stack := aString! !

!RsrRemoteError methodsFor!
originalClassName: aSymbol	originalClassName := aSymbol! !

!RsrRemoteError methodsFor!
stack	^stack! !

!RsrCycleDetected methodsFor!
messageText	^'Cycle detected on: ', object printString! !

!RsrCycleDetected methodsFor!
object: anObject	object := anObject! !

!RsrDeliverResponse methodsFor!
snapshots: anArrayOfSnapshots	snapshots := anArrayOfSnapshots! !

!RsrDeliverResponse methodsFor!
snapshots	^snapshots! !

!RsrDeliverResponse methodsFor!
response	^self responseReference! !

!RsrDeliverResponse methodsFor!
responseReference	^responseReference! !

!RsrDeliverResponse methodsFor!
executeFor: aConnection	| pendingMessage result |	pendingMessage := aConnection pendingMessages		removeKey: self transaction		ifAbsent:			[^self error: 'Handle unknown transaction'].	[self snapshots do: [:each | each reifyIn: aConnection].	result := self responseReference resolve: aConnection.	result first == #fulfill		ifTrue: [pendingMessage promise fulfill: result last]		ifFalse: [pendingMessage promise break: result last]]		on: Error		do: [:ex | pendingMessage promise fulfill: ex copy]! !

!RsrDeliverResponse methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeDeliverResponse: self		onto: aStream! !

!RsrDeliverResponse methodsFor!
response: anObject	^self responseReference: anObject! !

!RsrDeliverResponse methodsFor!
transaction	^transaction! !

!RsrDeliverResponse methodsFor!
reportOn: aLog	aLog debug: 'RsrDeliverResponse(', self response class name, ')'! !

!RsrDeliverResponse methodsFor!
responseReference: aReference	responseReference := aReference! !

!RsrDeliverResponse methodsFor!
transaction: aTransactionId	transaction := aTransactionId! !

!RsrRemoteException methodsFor!
stack: aString	stack := aString! !

!RsrRemoteException methodsFor!
tag	^tag! !

!RsrRemoteException methodsFor!
tag: aString	tag := aString! !

!RsrRemoteException methodsFor!
messageText: aString	messageText := aString! !

!RsrRemoteException methodsFor!
exceptionClassName	^exceptionClassName! !

!RsrRemoteException methodsFor!
exceptionClassName: aSymbol	exceptionClassName := aSymbol! !

!RsrRemoteException methodsFor!
isRemoteException	"This is a RemoteException reason"	^true! !

!RsrRemoteException methodsFor!
messageText	^messageText! !

!RsrRemoteException methodsFor!
stack	^stack! !

!RsrChannel methodsFor!
log	^self connection log! !

!RsrChannel methodsFor!
connection	^connection! !

!RsrChannel methodsFor!
close	"Ensure the channel is closed to further communication."	^self subclassResponsibility! !

!RsrChannel methodsFor!
isOpen	"Report whether the Channel is open between Connections."	^self subclassResponsibility! !

!RsrChannel methodsFor!
genericError: anError	^self connection unknownError: anError! !

!RsrChannel methodsFor!
open	"Ensure the channel is open and ready for communication."	^self subclassResponsibility! !

!RsrChannel methodsFor!
send: aCommand	"Send the provided command over the channel."	^self subclassResponsibility! !

!RsrChannel methodsFor!
received: aCommand	"A command has come in over the channel. Propogate it to the Connection."	self connection _receivedCommand: aCommand! !

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
isResolved	"Report if the receiver is currently resolved."	^self isFulfilled or: [self isBroken]! !

!RsrPromise methodsFor!
when: aWhenBlockcatch: aCatchBlock	"Activate an appropriate block when the receiver is resolved.	If the receiver is currently resolved, schedule the block activation.	The block is activated in a new thread. The thread is not given any specific	error handler.	<aWhenBlock> will be sent #value: with the same value provided to #fulfill:.	<aCatchBlock> will be sent #value: with the same reason provided to #break:."	| action shouldNotifyActions |	action := RsrPromiseResolutionAction		when: aWhenBlock		catch: aCatchBlock.	mutex		critical:			[shouldNotifyActions := self isResolved.			resolutionActions add: action].	shouldNotifyActions ifTrue: [self notifyActions]! !

!RsrPromise methodsFor!
wait	"Wait for a the receiver to be Resolved.	If fulfilled - return the fulfillment value.	If broken - raise an RsrBrokenPromise exception w/ the reason."	self waitForResolution.	^self isBroken		ifTrue: [RsrBrokenPromise signalReason: value]		ifFalse: [value]! !

!RsrPromise methodsFor!
fulfill: anObject	"Fulfill the receiver and notify any observers."	mutex		critical:			[self assertNotResolved.			value := anObject.			state := #Fulfilled].	self notifyActions.	resolvedMutex signal! !

!RsrPromise methodsFor!
notifyActions	"Activate any registered action's fulfillment blocks.	Ensure that they are activated only once and that	future actions are allowed."	| actions |	mutex		critical:			[actions := resolutionActions.			resolutionActions := OrderedCollection new].	actions		do:			[:each |			self isFulfilled				ifTrue: [RsrProcessModel fork: [each when value: value]]				ifFalse: [RsrProcessModel fork: [each catch value: value]]]! !

!RsrPromise methodsFor!
value	"Alias for #wait"	^self wait! !

!RsrPromise methodsFor!
initialize	super initialize.	mutex := Semaphore forMutualExclusion.	resolvedMutex := Semaphore new.	state := #PendingResolution.	resolutionActions := OrderedCollection new! !

!RsrPromise methodsFor!
break: aReason	"Notify the receiver's observers that the Promise will not be fulfilled."	mutex		critical:			[self assertNotResolved.			value := aReason.			state := #Broken].	self notifyActions.	resolvedMutex signal! !

!RsrPromise methodsFor!
isBroken	"Report if the receiver is currently broken"	^state == #Broken! !

!RsrPromise methodsFor!
isFulfilled	"Report is the receiver is currently fulfilled"	^state == #Fulfilled! !

!RsrPromise methodsFor!
assertNotResolved	self isResolved		ifTrue: [RsrAlreadyResolved signal].! !

!RsrPromise methodsFor!
waitForResolution	"There doesn't seem to be a great way to implement this method.	The ensure below is generally safe but does have a side-effect of signaling	the mutex when the process is terminated while waiting.	Removing the ensure allows the signal to be lost if the process is terminated	just after #wait but before #signal is processed.	In order to solve this, the loop verifies the promise is actually resolved before	continuing."	self isResolved		ifTrue: [^self].	[[self isResolved] whileFalse: [resolvedMutex wait]] ensure: [resolvedMutex signal]! !

!RsrEncoder methodsFor!
encodeControlWord: anIntegeronto: aStream	| encodedInteger encodedBytes |	(anInteger between: self controlWordMin and: self controlWordMax)		ifFalse: [self error: anInteger printString, ' is outside the supported size of a control word.'].	encodedInteger := (anInteger positive		ifTrue: [anInteger]		ifFalse: [(2 raisedTo: 64) + anInteger]).	encodedBytes := self		integerAsByteArray: encodedInteger		ofSize: self sizeOfInteger.	aStream nextPutAll: encodedBytes! !

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
encodeSendMessage: aSendMessageonto: aStream	self		encodeControlWord: self sendMessageCommand		onto: aStream.	self		encodeControlWord: aSendMessage transaction		onto: aStream.	self		encodeControlWord: aSendMessage snapshots size		onto: aStream.	aSendMessage snapshots		do:			[:each |			self				encodeServiceSnapshot: each				onto: aStream].	self		encodeReference:  aSendMessage receiverReference		onto: aStream.	self		encodeReference: aSendMessage selectorReference		onto: aStream.	self		encodeControlWord: aSendMessage argumentReferences size		onto: aStream.	aSendMessage argumentReferences		do:			[:each |			self				encodeReference: each				onto: aStream]! !

!RsrEncoder methodsFor!
encodeSendMessage: aSendMessage	^ByteArray streamContents: [:stream | self encodeSendMessage: aSendMessage onto: stream]! !

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
registerWith: aConnection	aConnection _ensureRegistered: self! !

!RsrService methodsFor!
_id	^_id! !

!RsrService methodsFor!
debug: anExceptionraisedDuring: aMessageSendanswerUsing: aResolver	^nil! !

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
synchronize	"Synchronize the service w/ its peer."	remoteSelf == nil		ifFalse: [remoteSelf _synchronize wait]! !

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
ensureRegistered: aService	self connection _ensureRegistered: aService.	aService isServer		ifTrue: [self connection _stronglyRetain: aService]! !

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