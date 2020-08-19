| package |
package := Package name: 'RemoteServiceReplication'.
package paxVersion: 1; basicComment: ''.

package classNames
	add: #RsrCommandSink;
	add: #RsrUnknownOID;
	add: #RsrRetainObject;
	add: #RsrPendingMessage;
	add: #RsrCodec;
	add: #RsrLogSink;
	add: #RsrServiceFactoryServer;
	add: #RsrAcceptConnection;
	add: #RsrStream;
	add: #RsrDeliverErrorResponse;
	add: #RsrNumericSpigot;
	add: #RsrCommandSource;
	add: #RsrService;
	add: #RsrSendMessage;
	add: #RsrPromise;
	add: #RsrDecoder;
	add: #RsrCustomSink;
	add: #RsrInitiateConnection;
	add: #RsrCycleDetected;
	add: #RsrDeliverResponse;
	add: #RsrThreadSafeNumericSpigot;
	add: #RsrDispatchEventLoop;
	add: #RsrServiceFactory;
	add: #RsrConnection;
	add: #RsrRetainAnalysis;
	add: #RsrEncoder;
	add: #RsrTranscriptSink;
	add: #RsrEventLoop;
	add: #RsrRemoteError;
	add: #RsrReleaseObjects;
	add: #RsrObjectCache;
	add: #RsrBufferedSocketStream;
	add: #RsrLog;
	add: #RsrServiceFactoryClient;
	add: #RsrConnectionSpecification;
	add: #RsrSocketStream;
	add: #RsrCommand;
	add: #RsrLogWithPrefix;
	yourself.

package methodNames
	add: #RsrForwarder -> #doesNotUnderstand:;
	add: #RsrForwarder -> #_service:;
	add: 'RsrForwarder class' -> #on:;
	add: 'RsrServiceSpecies class' -> #reflectedVariablesFor:;
	add: 'RsrServiceSpecies class' -> #reflectedVariableIndicesFor:do:;
	add: 'RsrServiceSpecies class' -> #reflectedVariablesFor:do:;
	add: 'RsrSpecies class' -> #speciesOf:;
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
	subclass: #RsrCodec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCodec categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrCommand
	instanceVariableNames: 'encoding'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCommand categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrConnection
	instanceVariableNames: 'isOpen transactionSpigot commandWriter commandReader registry objectCache socket stream pendingMessages dispatcher oidSpigot serviceFactory log closeSemaphore'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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
	subclass: #RsrEventLoop
	instanceVariableNames: 'process connection state'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrEventLoop categoriesForClass!RemoteServiceReplication! !

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
	subclass: #RsrObjectCache
	instanceVariableNames: 'storage'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrObjectCache categoriesForClass!RemoteServiceReplication! !

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

RsrObject
	subclass: #RsrRetainAnalysis
	instanceVariableNames: 'roots services inFlight connection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRetainAnalysis categoriesForClass!RemoteServiceReplication! !

RsrAbstractService
	subclass: #RsrService
	instanceVariableNames: '_id _connection remoteSelf'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrService comment: 'I represent a class of Objects that know offer Rsr Services.'!
!RsrService categoriesForClass!RemoteServiceReplication! !

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

RsrEventLoop
	subclass: #RsrCommandSink
	instanceVariableNames: 'queue'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCommandSink categoriesForClass!RemoteServiceReplication! !

RsrEventLoop
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
	instanceVariableNames: 'registry connection decodeCommandMap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDecoder categoriesForClass!RemoteServiceReplication! !

RsrCommand
	subclass: #RsrDeliverErrorResponse
	instanceVariableNames: 'transaction originalClass remoteError'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDeliverErrorResponse categoriesForClass!RemoteServiceReplication! !

RsrCommand
	subclass: #RsrDeliverResponse
	instanceVariableNames: 'transaction response roots retainList'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDeliverResponse categoriesForClass!RemoteServiceReplication! !

RsrEventLoop
	subclass: #RsrDispatchEventLoop
	instanceVariableNames: 'queue'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDispatchEventLoop categoriesForClass!RemoteServiceReplication! !

RsrCodec
	subclass: #RsrEncoder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrEncoder categoriesForClass!RemoteServiceReplication! !

RsrConnectionSpecification
	subclass: #RsrInitiateConnection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrInitiateConnection comment: 'This class is responsible for initating a new RsrConnection. Sending #connect will result in an attempt to connect to the specified host and port. #connect is responsible for initating the attempted connection. If successful, an instance of RsrConnection is returned as a result.Example: | initiator |initiator := RsrInitiateConnection	host: ''127.0.0.1''	port: 51820.^initiator connect'!
!RsrInitiateConnection categoriesForClass!RemoteServiceReplication! !

RsrCommand
	subclass: #RsrReleaseObjects
	instanceVariableNames: 'oids'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrReleaseObjects categoriesForClass!RemoteServiceReplication! !

RsrCommand
	subclass: #RsrRetainObject
	instanceVariableNames: 'object'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRetainObject categoriesForClass!RemoteServiceReplication! !

RsrCommand
	subclass: #RsrSendMessage
	instanceVariableNames: 'transaction receiver selector arguments retainList'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSendMessage categoriesForClass!RemoteServiceReplication! !

RsrService
	subclass: #RsrServiceFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceFactory categoriesForClass!RemoteServiceReplication! !

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

RsrError
	subclass: #RsrUnknownOID
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrUnknownOID categoriesForClass!RemoteServiceReplication! !

!RsrForwarder class methodsFor!
on: anRsrObject	| instance |	instance := self new.	instance _service: anRsrObject.	^instance! !

!RsrServiceSpecies class methodsFor!
reflectedVariablesFor: aService	| currentClass variables |	variables := OrderedCollection new.	currentClass := aService class templateClass.	[currentClass == RsrService]		whileFalse:			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].			currentClass := currentClass superclass].	^variables! !

!RsrServiceSpecies class methodsFor!
reflectedVariableIndicesFor: aServicedo: aBlock	| allVariables |	allVariables := aService class allInstVarNames.	(self reflectedVariablesFor: aService)		do:			[:varName | | index |			index := allVariables indexOf: varName.			aBlock value: index]! !

!RsrServiceSpecies class methodsFor!
reflectedVariablesFor: aServicedo: aBlock	self		reflectedVariableIndicesFor: aService		do: [:index | aBlock value: (aService instVarAt: index)]! !

!RsrSpecies class methodsFor!
speciesOf: anObject	(anObject isKindOf: RsrService)		ifTrue: [^RsrServiceSpecies].	anObject == true		ifTrue: [^RsrTrueSpecies].	anObject == false		ifTrue: [^RsrFalseSpecies].	(anObject isKindOf: Integer)		ifTrue: [^anObject positive ifTrue: [RsrPositiveIntegerSpecies] ifFalse: [RsrNegativeIntegerSpecies]].	^self speciesMapping		at: anObject class		ifAbsent: [RsrSpecies nullSpecies]! !

!RsrForwarder methodsFor!
doesNotUnderstand: aMessage	| promise |	promise := _service _connection		_sendMessage: aMessage		to: _service.	^promise value! !

!RsrForwarder methodsFor!
_service: aService	_service := aService! !

!RsrRemoteError class methodsFor!
from: anException	| tag |	tag := anException tag		ifNotNil:			[[anException tag asString]				on: Error				do: [:ex | ex return: 'Unable to pack #tag containing an instance of ', anException tag class name]].	^self new		originalClassName: anException class name;		tag: tag;		messageText: anException messageText;		stack: RsrProcessModel currentStackDump;		yourself! !

!RsrDeliverResponse class methodsFor!
transaction: aTransactionIderror: anExceptionroots: anArray	^self new		transaction: aTransactionId;		errorName: anException class name;		response: anException messageText;		roots: anArray;		yourself! !

!RsrDeliverResponse class methodsFor!
transaction: aTransactionIdresponse: anObjectroots: anArray	^self new		transaction: aTransactionId;		response: anObject;		roots: anArray;		yourself! !

!RsrServiceFactory class methodsFor!
templateClassName	^#RsrServiceFactory! !

!RsrReleaseObjects class methodsFor!
oids: anArray	^self new		oids: anArray;		yourself! !

!RsrRetainObject class methodsFor!
object: anRsrObject	^self new		object: anRsrObject;		yourself! !

!RsrRetainObject class methodsFor!
object: anRsrObjectencoding: aByteArray	^self new		object: anRsrObject;		encoding: aByteArray;		yourself! !

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

!RsrSendMessage class methodsFor!
transaction: aTransactionIdreceiver: aServiceselector: aSelectorarguments: anArray	^self new		transaction: aTransactionId;		receiver: aService;		selector: aSelector;		arguments: anArray;		yourself! !

!RsrEventLoop class methodsFor!
on: aConnection	^self new		connection: aConnection;		yourself! !

!RsrBufferedSocketStream class methodsFor!
on: aSocketStream	^self new		stream: aSocketStream;		yourself! !

!RsrCustomSink class methodsFor!
action: aBlock	^self new		action: aBlock;		yourself! !

!RsrAcceptConnection class methodsFor!
wildcardAddress	^'0.0.0.0'! !

!RsrAcceptConnection class methodsFor!
port: aPortInteger	^self		host: self wildcardAddress		port: aPortInteger! !

!RsrLogWithPrefix class methodsFor!
prefix: aStringlog: aLog	^self new		prefix: aString;		log: aLog;		yourself! !

!RsrLogWithPrefix class methodsFor!
log: aLog	^self new		log: aLog;		yourself! !

!RsrPendingMessage class methodsFor!
services: aListpromise: aPromise	^self new		services: aList;		promise: aPromise;		yourself! !

!RsrStream class methodsFor!
on: aStream	^self new		stream: aStream;		yourself! !

!RsrCycleDetected class methodsFor!
signal: anObject	^self new		object: anObject;		signal! !

!RsrRetainAnalysis class methodsFor!
roots: anArrayconnection: aConnection	^self new		roots: anArray;		connection: aConnection;		yourself! !

!RsrConnection class methodsFor!
new	self error: 'Instance creation via #new is unsupported'! !

!RsrConnection class methodsFor!
socket: aSockettransactionSpigot: aNumericSpigotoidSpigot: anOidSpigot	^super new		socket: aSocket;		transactionSpigot: aNumericSpigot;		oidSpigot: anOidSpigot;		yourself! !

!RsrConnection class methodsFor!
acceptOn: aPortNumber	| acceptor |	self deprecated: 'RsrConnection class>>#acceptOn: replaced by RsrAcceptConnection.'.	acceptor := RsrAcceptConnection port: aPortNumber.	^acceptor waitForConnection! !

!RsrConnection class methodsFor!
connectToHost: aHostnameport: aPortNumber	| initiator |	self deprecated: 'RsrConnection class>>#connectToHost:port: replaced by RsrInitiateConnection.'.	initiator := RsrInitiateConnection		host: aHostname		port: aPortNumber.	^initiator connect! !

!RsrDeliverErrorResponse class methodsFor!
transaction: aTransactionIdremoteError: anException	^self new		transaction: aTransactionId;		remoteError: anException;		yourself! !

!RsrDecoder class methodsFor!
registry: anRsrRegistryconnection: aConnection	^self new		registry: anRsrRegistry;		connection: aConnection;		yourself! !

!RsrSocketStream class methodsFor!
on: anRsrSocket	^self new		socket: anRsrSocket;		yourself! !

!RsrConnectionSpecification class methodsFor!
host: hostnameOrAddressport: port	^self new		host: hostnameOrAddress;		port: port;		yourself! !

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

!RsrRemoteError methodsFor!
originalClassName	^originalClassName! !

!RsrRemoteError methodsFor!
stack: aString	stack := aString! !

!RsrRemoteError methodsFor!
originalClassName: aSymbol	originalClassName := aSymbol! !

!RsrRemoteError methodsFor!
stack	^stack! !

!RsrDeliverResponse methodsFor!
response	^response! !

!RsrDeliverResponse methodsFor!
writeUsing: aCommandWriter	retainList do: [:each | each writeUsing: aCommandWriter].	aCommandWriter write: encoding! !

!RsrDeliverResponse methodsFor!
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeDeliverResponse: self! !

!RsrDeliverResponse methodsFor!
executeFor: aConnection	| pendingMessage |	pendingMessage := aConnection pendingMessages		removeKey: transaction		ifAbsent:			[^self error: 'Handle unknown transaction'].	pendingMessage promise fulfill: response.	aConnection objectCache reset! !

!RsrDeliverResponse methodsFor!
response: anObject	response := anObject! !

!RsrDeliverResponse methodsFor!
transaction	^transaction! !

!RsrDeliverResponse methodsFor!
roots	^roots! !

!RsrDeliverResponse methodsFor!
reportOn: aLog	aLog debug: 'RsrDeliverResponse(', self response class name, ')'! !

!RsrDeliverResponse methodsFor!
sendOver: aConnection	| analysis |	analysis := RsrRetainAnalysis		roots: roots		connection: aConnection.	analysis perform.	retainList := analysis retainCommands.	self encodeUsing: aConnection encoder.	aConnection commandWriter enqueue: self! !

!RsrDeliverResponse methodsFor!
transaction: aTransactionId	transaction := aTransactionId! !

!RsrDeliverResponse methodsFor!
roots: anArray	roots := anArray! !

!RsrEncoder methodsFor!
encodeObject: anObject	^ByteArray		streamContents:			[:stream |			self				encodeObject: anObject				onto: stream]! !

!RsrEncoder methodsFor!
speciesMapping	"Return a mapping between the native class and their associated RsrSpecies"	^RsrSpecies speciesMapping! !

!RsrEncoder methodsFor!
speciesOf: anObject		^RsrSpecies speciesOf: anObject! !

!RsrEncoder methodsFor!
encodeControlWord: anIntegeronto: aStream	| encodedInteger encodedBytes |	(anInteger between: self controlWordMin and: self controlWordMax)		ifFalse: [self error: anInteger printString, ' is outside the supported size of a control word.'].	encodedInteger := (anInteger positive		ifTrue: [anInteger]		ifFalse: [(2 raisedTo: 64) + anInteger]).	encodedBytes := self		integerAsByteArray: encodedInteger		ofSize: self sizeOfInteger.	aStream nextPutAll: encodedBytes! !

!RsrEncoder methodsFor!
encodeRetainObject: aRetainObject	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self retainObjectIdentifier				onto: stream.			self				encodeObject: aRetainObject object				onto: stream]! !

!RsrEncoder methodsFor!
encodeDeliverErrorResponse: aDeliverErrorResponse	| error |	error := aDeliverErrorResponse remoteError.	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self deliverErrorResponseCommand				onto: stream.			self				encodeControlWord: aDeliverErrorResponse transaction				onto: stream.			self				encodeReferenceOf: error originalClassName				onto: stream.			self				encodeReferenceOf: error tag				onto: stream.			self				encodeReferenceOf: error messageText				onto: stream.			self				encodeReferenceOf: error stack				onto: stream]! !

!RsrEncoder methodsFor!
encodeReferenceOf: anObjectonto: aStream	| species |	species := self speciesOf: anObject.	species		encodeReference: anObject		using: self		onto: aStream! !

!RsrEncoder methodsFor!
retainObjectIdentifier	^0! !

!RsrEncoder methodsFor!
encodeDeliverResponse: aDeliverResponse	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self deliverResponseCommand				onto: stream.			self				encodeControlWord: aDeliverResponse transaction				onto: stream.			self				encodeReferenceOf: aDeliverResponse response				onto: stream]! !

!RsrEncoder methodsFor!
encodeSendMessage: aSendMessage	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self sendMessageIdentifier				onto: stream.			self				encodeControlWord: aSendMessage transaction				onto: stream.			self				encodeControlWord: aSendMessage arguments size				onto: stream.			self				encodeReferenceOf: aSendMessage receiver				onto: stream.			self				encodeReferenceOf: aSendMessage selector				onto: stream.			aSendMessage arguments				do:					[:each |					self						encodeReferenceOf: each						onto: stream]]! !

!RsrEncoder methodsFor!
sendMessageIdentifier	^1! !

!RsrEncoder methodsFor!
encodeObject: anObjectonto: aStream	(self speciesOf: anObject)		encode: anObject		using: self		on: aStream! !

!RsrEncoder methodsFor!
encodeReleaseObjects: aReleaseObject	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self releaseObjectsCommand				onto: stream.			self				encodeControlWord: aReleaseObject oids size				onto: stream.			aReleaseObject oids				do:					[:oid |					self						encodeControlWord: oid						onto: stream]]! !

!RsrEncoder methodsFor!
isImmediate: anObject	^self speciesMapping includesKey: anObject class! !

!RsrEncoder methodsFor!
integerAsByteArray: anIntegerofSize: aNumberOfBytes	| bytes int |	bytes := ByteArray new: aNumberOfBytes.	int := anInteger.	aNumberOfBytes		to: 1		by: -1		do:			[:i | | byte |			byte := int bitAnd: 16rFF.			int := int bitShift: -8.			bytes at: i put: byte].	int ~= 0		ifTrue: [self error: 'Loss of precision detected'].	^bytes! !

!RsrInitiateConnection methodsFor!
connect	| socket connection |	socket := self socketClass new.	socket		connectToHost: self host		port: self port.	connection := RsrConnection		socket: socket		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated		oidSpigot: RsrThreadSafeNumericSpigot naturals negated.	^connection open! !

!RsrCommandSink methodsFor!
write: aByteArray	self stream nextPutAll: aByteArray! !

!RsrCommandSink methodsFor!
writeCommand: aCommand	self report: aCommand.	aCommand writeUsing: self! !

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
executeCycle	[| command |	command := queue next.	command == self stopToken		ifTrue: [^self].	self writeCommand: command.	(queue size = 0)		ifTrue: [self flush]]		on: RsrSocketClosed		do:			[:ex |			self reportException: ex.			self connection disconnected]! !

!RsrReleaseObjects methodsFor!
reportOn: aLog	aLog debug: 'RsrReleaseObjects(', self oids printString, ')'! !

!RsrReleaseObjects methodsFor!
executeFor: aConnection	| registry |	registry := aConnection registry.	oids do: [:oid | registry cleanupEntryFor: oid]! !

!RsrReleaseObjects methodsFor!
oids	^oids! !

!RsrReleaseObjects methodsFor!
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeReleaseObjects: self! !

!RsrReleaseObjects methodsFor!
oids: anArray	oids := anArray! !

!RsrRetainObject methodsFor!
reportOn: aLog	aLog debug: 'RsrRetainObject(', self object class name, ')'! !

!RsrRetainObject methodsFor!
executeFor: aConnection	aConnection objectCache add: object! !

!RsrRetainObject methodsFor!
= anEncodedObject	self == anEncodedObject		ifTrue: [^true].	self class == anEncodedObject class		ifFalse: [^false].	^self object = anEncodedObject object		and: [self encoding = anEncodedObject encoding]! !

!RsrRetainObject methodsFor!
hash	^self object hash! !

!RsrRetainObject methodsFor!
object: anObject	object := anObject! !

!RsrRetainObject methodsFor!
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeRetainObject: self! !

!RsrRetainObject methodsFor!
object	^ object! !

!RsrRetainObject methodsFor!
writeUsing: aCommandWriter	super writeUsing: aCommandWriter! !

!RsrService methodsFor!
isClient	^self class isClientClass! !

!RsrService methodsFor!
reflectedVariableNames	^RsrServiceSpecies reflectedVariablesFor: self! !

!RsrService methodsFor!
registerWith: aConnection	aConnection serviceFactory mirror: self! !

!RsrService methodsFor!
_id	^_id! !

!RsrService methodsFor!
_id: anRsrIdconnection: aConnection	_id := anRsrId.	_connection := aConnection.	remoteSelf := aConnection _forwarderClass on: self! !

!RsrService methodsFor!
isMirrored	^_connection ~~ nil! !

!RsrService methodsFor!
_synchronize	"Return self to synchronize with the remote peer"	^self! !

!RsrService methodsFor!
isServer	^self class isServerClass! !

!RsrService methodsFor!
_connection	^_connection! !

!RsrService methodsFor!
synchronize	remoteSelf == nil		ifFalse: [remoteSelf _synchronize]! !

!RsrService methodsFor!
isNotMirrored	^self isMirrored not! !

!RsrSendMessage methodsFor!
receiver	^ receiver! !

!RsrSendMessage methodsFor!
writeUsing: aCommandWriter	retainList do: [:each | each writeUsing: aCommandWriter].	aCommandWriter write: encoding! !

!RsrSendMessage methodsFor!
arguments	^ arguments! !

!RsrSendMessage methodsFor!
logException: anExceptionto: aLog	| message |	message := String		streamContents:			[:stream |			stream				nextPutAll: receiver class name;				nextPutAll: '>>';				nextPutAll: selector;				nextPutAll: ' due to: ';				nextPutAll: anException description].	aLog error: message! !

!RsrSendMessage methodsFor!
arguments: anObject	arguments := anObject! !

!RsrSendMessage methodsFor!
executeFor: aConnection	| result response |	[result := receiver		perform: selector		withArguments: arguments.	aConnection objectCache reset.	response := RsrDeliverResponse		transaction: transaction		response: result		roots: (Array with: receiver with: result).	response sendOver: aConnection]		on: Error		do:			[:ex |			self				logException: ex				to: aConnection log.			(RsrDeliverErrorResponse transaction: transaction remoteError: (RsrRemoteError from: ex)) sendOver: aConnection]! !

!RsrSendMessage methodsFor!
selector	^ selector! !

!RsrSendMessage methodsFor!
encodeUsing: anEncoder	encoding := anEncoder encodeSendMessage: self! !

!RsrSendMessage methodsFor!
receiver: anObject	receiver := anObject! !

!RsrSendMessage methodsFor!
roots	^(Array with: receiver with: selector) ,  arguments! !

!RsrSendMessage methodsFor!
reportOn: aLog	aLog debug: 'RsrSendMessage(', self receiver class name, '>>', self selector, ')'! !

!RsrSendMessage methodsFor!
selector: anObject	selector := anObject! !

!RsrSendMessage methodsFor!
sendOver: aConnection	| analysis promise pendingMessage |	analysis := RsrRetainAnalysis		roots: self roots		connection: aConnection.	analysis perform.	retainList := analysis retainCommands.	self encodeUsing: aConnection encoder.	promise := RsrPromise new.	pendingMessage := RsrPendingMessage		services: analysis services		promise: promise.	aConnection pendingMessages		at: transaction		put: pendingMessage.	aConnection commandWriter enqueue: self.	^promise! !

!RsrSendMessage methodsFor!
transaction	^ transaction! !

!RsrSendMessage methodsFor!
transaction: anObject	transaction := anObject! !

!RsrServiceFactoryServer methodsFor!
create: aResponsibility	| abstractClass |	abstractClass := RsrClassResolver classNamed: aResponsibility.	^abstractClass serverClass new! !

!RsrServiceFactoryServer methodsFor!
mirror: aService	^aService! !

!RsrServiceFactoryClient methodsFor!
mirror: aService	^remoteSelf mirror: aService! !

!RsrServiceFactoryClient methodsFor!
serviceFor: aResponsibility	^remoteSelf create: aResponsibility! !

!RsrEventLoop methodsFor!
stop	self isActive ifFalse: [^self].	state := self stoppedState.	self connection close.	self stream close! !

!RsrEventLoop methodsFor!
log: aString	self log debug: aString! !

!RsrEventLoop methodsFor!
executeCycle	self subclassResponsibility! !

!RsrEventLoop methodsFor!
isProcessActive	^process ~~ nil! !

!RsrEventLoop methodsFor!
start	state := self runningState.	process := RsrProcessModel		fork: [self runLoop.				process := nil]		at: self priority! !

!RsrEventLoop methodsFor!
stoppedState	^#Stop! !

!RsrEventLoop methodsFor!
initialize	super initialize.	state := self stoppedState! !

!RsrEventLoop methodsFor!
priority	^Processor lowIOPriority! !

!RsrEventLoop methodsFor!
connection: anObject	connection := anObject! !

!RsrEventLoop methodsFor!
runningState	^#Running! !

!RsrEventLoop methodsFor!
report: aCommand	aCommand reportOn: self log! !

!RsrEventLoop methodsFor!
runLoop	[self isActive]		whileTrue:			[[self executeCycle]				on: Error				do:					[:ex |					self reportException: ex.					self connection unknownError: ex]]! !

!RsrEventLoop methodsFor!
isActive	^state == self runningState! !

!RsrEventLoop methodsFor!
log	^RsrLogWithPrefix		prefix: self class name asString		log: self connection log! !

!RsrEventLoop methodsFor!
reportException: anException	self log: '', self class name, '/', anException description! !

!RsrEventLoop methodsFor!
connection	^connection! !

!RsrEventLoop methodsFor!
stream	^self connection stream! !

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

!RsrCustomSink methodsFor!
action: aBlock	action := aBlock! !

!RsrCustomSink methodsFor!
action	^action! !

!RsrCustomSink methodsFor!
write: aMessage	self action value: aMessage! !

!RsrAcceptConnection methodsFor!
cancelWaitForConnection	listener ifNotNil: [:socket | socket close]! !

!RsrAcceptConnection methodsFor!
isWaitingForConnection	^listener ~~ nil! !

!RsrAcceptConnection methodsFor!
waitForConnection	| socket connection |	listener := self socketClass new.	[listener		bindAddress: self host		port: self port.	listener listen: 1.	socket := [listener accept]		on: RsrSocketClosed		do: [:ex | ex resignalAs: RsrWaitForConnectionCancelled new]]			ensure:				[listener close.				listener := nil].	connection := RsrConnection		socket: socket		transactionSpigot: RsrThreadSafeNumericSpigot naturals		oidSpigot: RsrThreadSafeNumericSpigot naturals.	^connection open! !

!RsrTranscriptSink methodsFor!
write: aMessageString	Transcript		show: aMessageString;		cr! !

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

!RsrObjectCache methodsFor!
initialize	super initialize.	self reset! !

!RsrObjectCache methodsFor!
add: anObject	storage add: anObject! !

!RsrObjectCache methodsFor!
reset	storage := IdentitySet new! !

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

!RsrLog methodsFor!
levelError	^1! !

!RsrLog methodsFor!
levelInfo	^3! !

!RsrLog methodsFor!
debug: aString	self verbosity >= self levelDebug		ifTrue: [	self log: aString level: #debug]! !

!RsrLog methodsFor!
levelTrace	^5! !

!RsrLog methodsFor!
log: aMessagelevel: aLevelString	| message |	message := RsrDateAndTimeSpecies now printString, '-', aLevelString, '-', aMessage.	sinks do: [:each | each write: message]! !

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

!RsrLogSink methodsFor!
write: aMessage	self subclassResponsibility! !

!RsrCommandSource methodsFor!
decoder	^self connection decoder! !

!RsrCommandSource methodsFor!
nextCommand	^self decoder decodeCommand: self stream! !

!RsrCommandSource methodsFor!
dispatcher	^self connection dispatcher! !

!RsrCommandSource methodsFor!
executeCycle	[| command |	command := self nextCommand.	self report: command.	self dispatcher dispatch: command]		on: RsrSocketClosed		do:			[:ex |			self reportException: ex.			self connection disconnected]! !

!RsrRetainAnalysis methodsFor!
services	^services! !

!RsrRetainAnalysis methodsFor!
speciesOf: anObject	^RsrSpecies speciesOf: anObject! !

!RsrRetainAnalysis methodsFor!
analyzeDictionary: aDictionary	self		analyzing: aDictionary		during:			[aDictionary				keysAndValuesDo:					[:key :value |					self						analyze: key;						analyze: value]].	^aDictionary! !

!RsrRetainAnalysis methodsFor!
analyzeImmediate: anObject	"Nothing to do for a generic immediate"	^anObject! !

!RsrRetainAnalysis methodsFor!
analyze: anObject	^(self speciesOf: anObject)		analyze: anObject		using: self! !

!RsrRetainAnalysis methodsFor!
ensureRegistered: aService	self connection ensureRegistered: aService! !

!RsrRetainAnalysis methodsFor!
retainCommands	^self services		collect:			[:service | | command |			command := RsrRetainObject object: service.			command encodeUsing: self encoder.			command]! !

!RsrRetainAnalysis methodsFor!
initialize	super initialize.	services := OrderedCollection new.	inFlight := IdentitySet new! !

!RsrRetainAnalysis methodsFor!
retain: aService	services add: aService! !

!RsrRetainAnalysis methodsFor!
encoder	^self connection encoder! !

!RsrRetainAnalysis methodsFor!
analyzeCollection: aCollection	self		analyzing: aCollection		during: [	aCollection do: [:each | self analyze: each]].	^aCollection! !

!RsrRetainAnalysis methodsFor!
analyzing: anObjectduring: aBlock	(inFlight includes: anObject)		ifTrue: [^RsrCycleDetected signal: anObject].	inFlight add: anObject.	aBlock value.	inFlight remove: anObject! !

!RsrRetainAnalysis methodsFor!
connection: aConnection	connection := aConnection! !

!RsrRetainAnalysis methodsFor!
roots	^roots! !

!RsrRetainAnalysis methodsFor!
analyzeService: aService	self ensureRegistered: aService.	self		analyzing: aService		during:			[RsrServiceSpecies				reflectedVariablesFor: aService				do: [:each | self analyze: each]].	self retain: aService! !

!RsrRetainAnalysis methodsFor!
roots: anObject	roots := anObject! !

!RsrRetainAnalysis methodsFor!
connection	^connection! !

!RsrRetainAnalysis methodsFor!
perform	roots do: [:each | self analyze: each]! !

!RsrCycleDetected methodsFor!
messageText	^'Cycle detected on: ', object printString! !

!RsrCycleDetected methodsFor!
object: anObject	object := anObject! !

!RsrConnection methodsFor!
serviceFor: aResponsibility	^self serviceFactory serviceFor: aResponsibility! !

!RsrConnection methodsFor!
close	isOpen		ifFalse: [^self].	isOpen := false.	commandReader stop.	commandWriter stop.	dispatcher stop.	pendingMessages do: [:each | each promise error: RsrConnectionClosed new].	commandReader := commandWriter := dispatcher := pendingMessages := objectCache := registry := nil.	closeSemaphore signal! !

!RsrConnection methodsFor!
log	^log! !

!RsrConnection methodsFor!
decoder	^RsrDecoder registry: registry connection: self! !

!RsrConnection methodsFor!
initialize	super initialize.	isOpen := false.	transactionSpigot := RsrThreadSafeNumericSpigot naturals.	objectCache := RsrObjectCache new.	pendingMessages := Dictionary new.	registry := RsrRegistry reapAction: [:oid | self releaseOid: oid].	log := RsrLog new! !

!RsrConnection methodsFor!
transactionSpigot	^transactionSpigot! !

!RsrConnection methodsFor!
stream	^stream! !

!RsrConnection methodsFor!
oidSpigot	^oidSpigot! !

!RsrConnection methodsFor!
isClosed	^self isOpen not! !

!RsrConnection methodsFor!
objectCache	^objectCache! !

!RsrConnection methodsFor!
disconnected	self log info: 'Disconnected'.	self close! !

!RsrConnection methodsFor!
oidSpigot: anIntegerSpigot	oidSpigot := anIntegerSpigot! !

!RsrConnection methodsFor!
commandReader	^commandReader! !

!RsrConnection methodsFor!
newTransactionId	^transactionSpigot next! !

!RsrConnection methodsFor!
commandWriter	^commandWriter! !

!RsrConnection methodsFor!
_sendMessage: aMessageto: aService"Open coordination window"	"Send dirty transitive closure of aRemoteMessage"	"Send DispatchMessage command""Coorination window closed"	"Return Promise"	| dispatchCommand |	isOpen		ifFalse: [self error: 'Connection is not open'].	dispatchCommand := RsrSendMessage		transaction: self newTransactionId		receiver: aService		selector: aMessage selector		arguments: aMessage arguments.	^dispatchCommand sendOver: self! !

!RsrConnection methodsFor!
dispatcher	^dispatcher! !

!RsrConnection methodsFor!
pendingMessages	^pendingMessages! !

!RsrConnection methodsFor!
registry	^registry! !

!RsrConnection methodsFor!
unknownError: anException	self close! !

!RsrConnection methodsFor!
ensureRegistered: aService	aService isMirrored		ifTrue:			[^aService _connection == self				ifTrue: [self]				ifFalse: [RsrAlreadyRegistered signalService: aService intendedConnection: self]].	aService		_id: oidSpigot next		connection: self.	self registry		serviceAt: aService _id		put: aService! !

!RsrConnection methodsFor!
encoder	^RsrEncoder new! !

!RsrConnection methodsFor!
waitUntilClose	closeSemaphore wait.	closeSemaphore signal! !

!RsrConnection methodsFor!
releaseOid: anOid	| command |	self isOpen		ifFalse: [^self].	self log trace: 'Cleaning up OID:', anOid printString.	command := RsrReleaseObjects oids: (Array with: anOid).	command encodeUsing: self encoder.	commandWriter enqueue: command! !

!RsrConnection methodsFor!
_forwarderClass	^RsrForwarder! !

!RsrConnection methodsFor!
serviceFactory	^serviceFactory! !

!RsrConnection methodsFor!
open	(isOpen := socket isConnected)		ifFalse: [^RsrConnectionClosed signal].	closeSemaphore := Semaphore new.	stream := RsrSocketStream on: socket.	dispatcher := RsrDispatchEventLoop on: self.	commandReader := RsrCommandSource on: self.	commandWriter := RsrCommandSink on: self.	dispatcher start.	commandReader start.	commandWriter start.	serviceFactory := RsrServiceFactory clientClass		_id: self oidSpigot next		connection: self.	registry		serviceAt: serviceFactory _id		put: serviceFactory! !

!RsrConnection methodsFor!
transactionSpigot: anObject	transactionSpigot := anObject! !

!RsrConnection methodsFor!
isOpen	^isOpen! !

!RsrConnection methodsFor!
socket: aSocket	socket := aSocket! !

!RsrThreadSafeNumericSpigot methodsFor!
initialize	super initialize.	mutex := Semaphore forMutualExclusion! !

!RsrThreadSafeNumericSpigot methodsFor!
next	^mutex critical: [super next]! !

!RsrCodec methodsFor!
sizeOfInteger	"Return the number of bytes used to encode an integer"	^8! !

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
retainObjectCommand	^0! !

!RsrDeliverErrorResponse methodsFor!
transaction	^transaction! !

!RsrDeliverErrorResponse methodsFor!
reportOn: aLog	aLog debug: 'RsrDeliverErrorResponse(', self remoteError class name, ')'! !

!RsrDeliverErrorResponse methodsFor!
executeFor: aConnection	| pendingMessage |	pendingMessage := aConnection pendingMessages		removeKey: transaction		ifAbsent: [^self error: 'Handle unknown transaction'].	pendingMessage promise error: self remoteError! !

!RsrDeliverErrorResponse methodsFor!
sendOver: aConnection	self encodeUsing: aConnection encoder.	aConnection commandWriter enqueue: self! !

!RsrDeliverErrorResponse methodsFor!
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeDeliverErrorResponse: self! !

!RsrDeliverErrorResponse methodsFor!
remoteError	^remoteError! !

!RsrDeliverErrorResponse methodsFor!
remoteError: aRemoteError	remoteError := aRemoteError! !

!RsrDeliverErrorResponse methodsFor!
transaction: anInteger	transaction := anInteger! !

!RsrDecoder methodsFor!
decodeDeliverResponse: aStream	| transaction response |	transaction := self decodeControlWord: aStream.	response := self decodeObjectReference: aStream.	^RsrDeliverResponse new		transaction: transaction;		response: response;		yourself! !

!RsrDecoder methodsFor!
lookupClass: aClassName	^RsrClassResolver classNamed: aClassName! !

!RsrDecoder methodsFor!
decodeService: aStream	^RsrServiceSpecies		decode: aStream		using: self! !

!RsrDecoder methodsFor!
decodeCommandMap	^decodeCommandMap ifNil: [self initializeDecodeCommandMap]! !

!RsrDecoder methodsFor!
decodeControlWord: aStream	| bytes unsignedResult |	bytes := aStream next: self sizeOfInteger.	unsignedResult := self bytesAsInteger: bytes.	^unsignedResult > self controlWordMax		ifTrue: [(2 raisedTo: 64) negated + unsignedResult]		ifFalse: [unsignedResult]! !

!RsrDecoder methodsFor!
decodeDeliverErrorResponse: aStream	| transaction originalClassName tag messageText stack error |	transaction := self decodeControlWord: aStream.	originalClassName := self decodeObjectReference: aStream.	tag := self decodeObjectReference: aStream.	messageText := self decodeObjectReference: aStream.	stack := self decodeObjectReference: aStream.	error := RsrRemoteError new		originalClassName: originalClassName;		tag: tag;		messageText: messageText;		stack: stack;		yourself.	^RsrDeliverErrorResponse new		transaction: transaction;		remoteError: error;		yourself! !

!RsrDecoder methodsFor!
initializeDecodeCommandMap	decodeCommandMap := Dictionary new.	decodeCommandMap		at: self retainObjectCommand put: #decodeRetainObject:;		at: self sendMessageCommand put: #decodeSendMessage:;		at: self deliverResponseCommand put: #decodeDeliverResponse:;		at: self releaseObjectsCommand put: #decodeReleaseObjects:;		at: self deliverErrorResponseCommand put: #decodeDeliverErrorResponse:.	^decodeCommandMap! !

!RsrDecoder methodsFor!
registry	^registry! !

!RsrDecoder methodsFor!
signalUnknownOID	RsrUnknownOID signal! !

!RsrDecoder methodsFor!
bytesAsInteger: bytes	| res |	res := 0.	bytes do: [:e | res := (res bitShift: 8) bitOr: e].	^res! !

!RsrDecoder methodsFor!
decodeReleaseObjects: aStream	| count oids |	count := self decodeControlWord: aStream.	oids := Array new: count.	1		to: count		do:			[:i | | oid |			oid := self decodeControlWord: aStream.			oids at: i put: oid].	^RsrReleaseObjects oids: oids! !

!RsrDecoder methodsFor!
decodeSendMessage: aStream	| transaction argCount receiverOID receiver selector arguments |	transaction := self decodeControlWord: aStream.	argCount := self decodeControlWord: aStream.	receiverOID := self decodeControlWord: aStream.	receiver := registry serviceAt: receiverOID ifAbsent: [^self signalUnknownOID].	selector := self decodeObjectReference: aStream.	arguments := (1 to: argCount) collect: [:each | self decodeObjectReference: aStream].	^RsrSendMessage		transaction: transaction		receiver: receiver		selector: selector		arguments: arguments! !

!RsrDecoder methodsFor!
registry: anRsrRegistry	registry := anRsrRegistry! !

!RsrDecoder methodsFor!
decodeCommand: aStream	"Decode an object from the stream"	| command decodeSelector |	command := self decodeControlWord: aStream.	decodeSelector := self decodeCommandMap		at: command		ifAbsent: [self error: 'Invalid command received'].	^self		perform: decodeSelector		with: aStream! !

!RsrDecoder methodsFor!
decodeRetainObject: aStream	^RsrRetainObject object: (self decodeService: aStream)! !

!RsrDecoder methodsFor!
connection: aConnection	connection := aConnection! !

!RsrDecoder methodsFor!
connection	^connection! !

!RsrDecoder methodsFor!
decodeObjectReference: aStream	| oid |	oid := self decodeControlWord: aStream.	oid = self immediateOID ifTrue: [^self decodeImmediateObject: aStream].	^registry serviceAt: oid ifAbsent: [self signalUnknownOID]! !

!RsrDecoder methodsFor!
decodeImmediateObject: aStream	| species |	species := self decodeControlWord: aStream.	^(RsrSpecies speciesList at: species + 1)		decodeReference: aStream		using: self! !

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

!RsrCommand methodsFor!
reportOn: aLog	self subclassResponsibility! !

!RsrCommand methodsFor!
encoding	^ encoding! !

!RsrCommand methodsFor!
executeFor: aConnection	self subclassResponsibility! !

!RsrCommand methodsFor!
sendOver: aConnection	"Do nothing unless a subclass offers a specialization"! !

!RsrCommand methodsFor!
writeUsing: aCommandWriter	aCommandWriter write: encoding! !

!RsrCommand methodsFor!
encodeUsing: anRsrEncoder	self subclassResponsibility! !

!RsrCommand methodsFor!
encoding: anObject	encoding := anObject! !

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

!RsrDispatchEventLoop methodsFor!
initialize	super initialize.	queue := SharedQueue new! !

!RsrDispatchEventLoop methodsFor!
stopToken	^self stoppedState! !

!RsrDispatchEventLoop methodsFor!
priority	^super priority - 1! !

!RsrDispatchEventLoop methodsFor!
stop	super stop.	queue nextPut: self stopToken! !

!RsrDispatchEventLoop methodsFor!
dispatch: aCommand	queue nextPut: aCommand! !

!RsrDispatchEventLoop methodsFor!
executeCycle	| item |	item := queue next.	item == self stopToken		ifTrue: [^self].	self report: item.	item executeFor: connection! !