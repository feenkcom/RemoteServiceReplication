| package |
package := Package name: 'RemoteServiceReplication'.
package paxVersion: 1; basicComment: ''.

package classNames
	add: #RsrService;
	add: #RsrStream;
	add: #RsrEncoder;
	add: #RsrThreadSafeNumericSpigot;
	add: #RsrSendMessage;
	add: #RsrUnknownOID;
	add: #RsrRetainAnalysis;
	add: #RsrDecoder;
	add: #RsrSocketStream;
	add: #RsrCycleDetected;
	add: #RsrNumericSpigot;
	add: #RsrCodec;
	add: #RsrLogWithPrefix;
	add: #RsrBufferedSocketStream;
	add: #RsrLog;
	add: #RsrPromise;
	add: #RsrConnection;
	add: #RsrTranscriptSink;
	add: #RsrRetainObject;
	add: #RsrCommandSink;
	add: #RsrServiceFactoryServer;
	add: #RsrLogSink;
	add: #RsrReleaseObjects;
	add: #RsrCommandSource;
	add: #RsrServiceFactory;
	add: #RsrDeliverResponse;
	add: #RsrObjectCache;
	add: #RsrDispatchEventLoop;
	add: #RsrAbstractServiceFactory;
	add: #RsrCommand;
	add: #RsrEventLoop;
	yourself.

package methodNames
	add: #RsrForwarder -> #doesNotUnderstand:;
	add: #RsrForwarder -> #_service:;
	add: 'RsrForwarder class' -> #on:;
	yourself.

package setPrerequisites: #('RemoteServiceReplication-Compatibility-Dolphin').

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
	instanceVariableNames: 'isOpen transactionSpigot commandWriter commandReader registry objectCache socket stream promises dispatcher oidSpigot serviceFactory log closeSemaphore'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrConnection categoriesForClass!RemoteServiceReplication! !

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
	subclass: #RsrPromise
	instanceVariableNames: 'mutex value error markerValue'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrPromise categoriesForClass!RemoteServiceReplication! !

RsrObject
	subclass: #RsrRetainAnalysis
	instanceVariableNames: 'roots retainCommands inFlight connection'
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

RsrService
	subclass: #RsrAbstractServiceFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrAbstractServiceFactory categoriesForClass!RemoteServiceReplication! !

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

RsrCodec
	subclass: #RsrDecoder
	instanceVariableNames: 'registry connection decodeCommandMap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDecoder categoriesForClass!RemoteServiceReplication! !

RsrCommand
	subclass: #RsrDeliverResponse
	instanceVariableNames: 'transaction errorName response roots retainList'
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

RsrAbstractServiceFactory
	subclass: #RsrServiceFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceFactory categoriesForClass!RemoteServiceReplication! !

RsrAbstractServiceFactory
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

!RsrForwarder methodsFor!
doesNotUnderstand: aMessage	| promise |	promise := _service _connection		_sendMessage: aMessage		to: _service.	^promise value! !

!RsrForwarder methodsFor!
_service: aService	_service := aService! !

!RsrService class methodsFor!
clientClassName	self subclassResponsibility! !

!RsrService class methodsFor!
_id: anIdconnection: aConnection	^super new		_id: anId connection: aConnection;		yourself! !

!RsrService class methodsFor!
_variablesToReflect	| currentClass variables |	variables := OrderedCollection new.	currentClass := self abstractClass.	[currentClass == RsrService]		whileFalse:			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].			currentClass := currentClass superclass].	^variables! !

!RsrService class methodsFor!
abstractClass	^RsrClassResolver classNamed: self abstractClassName! !

!RsrService class methodsFor!
isClientClass	^self name == self clientClassName! !

!RsrService class methodsFor!
isServerClass	^self name == self serverClassName! !

!RsrService class methodsFor!
abstractClassName	self subclassResponsibility! !

!RsrService class methodsFor!
serverClass	^RsrClassResolver classNamed: self serverClassName! !

!RsrService class methodsFor!
clientClass	^RsrClassResolver classNamed: self clientClassName! !

!RsrService class methodsFor!
serverClassName	self subclassResponsibility! !

!RsrService class methodsFor!
isAbstractClass	^self name == self abstractClassName! !

!RsrDeliverResponse class methodsFor!
transaction: aTransactionIderror: anExceptionroots: anArray	^self new		transaction: aTransactionId;		errorName: anException class name;		response: anException messageText;		roots: anArray;		yourself! !

!RsrDeliverResponse class methodsFor!
transaction: aTransactionIdresponse: anObjectroots: anArray	^self new		transaction: aTransactionId;		response: anObject;		roots: anArray;		yourself! !

!RsrBufferedSocketStream class methodsFor!
on: aSocketStream	^self new		stream: aSocketStream;		yourself! !

!RsrDecoder class methodsFor!
registry: anRsrRegistryconnection: aConnection	^self new		registry: anRsrRegistry;		connection: aConnection;		yourself! !

!RsrRetainObject class methodsFor!
object: anRsrObject	^self new		object: anRsrObject;		yourself! !

!RsrRetainObject class methodsFor!
object: anRsrObjectencoding: aByteArray	^self new		object: anRsrObject;		encoding: aByteArray;		yourself! !

!RsrCycleDetected class methodsFor!
signal: anObject	^self new		object: anObject;		signal! !

!RsrEventLoop class methodsFor!
on: aConnection	^self new		connection: aConnection;		yourself! !

!RsrSendMessage class methodsFor!
transaction: aTransactionIdreceiver: aServiceselector: aSelectorarguments: anArray	^self new		transaction: aTransactionId;		receiver: aService;		selector: aSelector;		arguments: anArray;		yourself! !

!RsrRetainAnalysis class methodsFor!
roots: anArrayconnection: aConnection	^self new		roots: anArray;		connection: aConnection;		yourself! !

!RsrReleaseObjects class methodsFor!
oids: anArray	^self new		oids: anArray;		yourself! !

!RsrLogWithPrefix class methodsFor!
prefix: aStringlog: aLog	^self new		prefix: aString;		log: aLog;		yourself! !

!RsrLogWithPrefix class methodsFor!
log: aLog	^self new		log: aLog;		yourself! !

!RsrConnection class methodsFor!
socket: aSockettransactionSpigot: aNumericSpigotoidSpigot: anOidSpigot	^super new		socket: aSocket;		transactionSpigot: aNumericSpigot;		oidSpigot: anOidSpigot;		yourself! !

!RsrConnection class methodsFor!
new	self error: 'Instance creation via #new is unsupported'! !

!RsrConnection class methodsFor!
connectTo: aPortNumberon: aHostname	| socket |	socket := RsrSocket new.	socket		connectTo: aPortNumber		on: aHostname.	^(self		socket: socket		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated		oidSpigot: RsrThreadSafeNumericSpigot naturals negated) open! !

!RsrConnection class methodsFor!
acceptOn: aPortNumber	| listener socket |	listener := RsrSocket new.	[listener listenOn: aPortNumber.	socket := listener accept]		ensure: [listener close].	^(self		socket: socket		transactionSpigot: RsrThreadSafeNumericSpigot naturals		oidSpigot: RsrThreadSafeNumericSpigot naturals) open! !

!RsrConnection class methodsFor!
connectionTimeout	^2! !

!RsrSocketStream class methodsFor!
on: anRsrSocket	^self new		socket: anRsrSocket;		yourself! !

!RsrStream class methodsFor!
on: aStream	^self new		stream: aStream;		yourself! !

!RsrAbstractServiceFactory class methodsFor!
clientClassName	^#RsrServiceFactory! !

!RsrAbstractServiceFactory class methodsFor!
abstractClassName	^#RsrServiceFactory! !

!RsrAbstractServiceFactory class methodsFor!
serverClassName	^#RsrServiceFactoryServer! !

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
next: aCount	| result |	result := Array new: aCount.	1 to: aCount do: [:i | result at: i put: self next].	^result! !

!RsrNumericSpigot methodsFor!
negated	^self class		start: current negated		step: step negated! !

!RsrNumericSpigot methodsFor!
next	| result |	result := current.	current := current + step.	^result! !

!RsrNumericSpigot methodsFor!
start: aNumber	current := aNumber! !

!RsrPromise methodsFor!
waitForFulfillment	self isFulfilled		ifTrue: [^self].	mutex wait.	mutex signal! !

!RsrPromise methodsFor!
value	self waitForFulfillment.	error isNil		ifFalse: [error copy signal].	^value! !

!RsrPromise methodsFor!
error: anException	self isFulfilled		ifTrue: [^self error: 'Promise value already set'].	error := anException.	mutex signal	! !

!RsrPromise methodsFor!
initialize	super initialize.	value := markerValue := Object new.	mutex := Semaphore new! !

!RsrPromise methodsFor!
isFulfilled	^value ~~ markerValue! !

!RsrPromise methodsFor!
fulfill: anObject	self isFulfilled		ifTrue: [^self error: 'Promise value already set'].	value := anObject.	mutex signal! !

!RsrDeliverResponse methodsFor!
roots: anArray	roots := anArray! !

!RsrDeliverResponse methodsFor!
response	^response! !

!RsrDeliverResponse methodsFor!
writeUsing: aCommandWriter	retainList do: [:each | each writeUsing: aCommandWriter].	aCommandWriter write: encoding! !

!RsrDeliverResponse methodsFor!
errorName: aSymbol	errorName := aSymbol! !

!RsrDeliverResponse methodsFor!
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeDeliverResponse: self! !

!RsrDeliverResponse methodsFor!
executeFor: aConnection	| promise |	promise := aConnection promises		removeKey: transaction		ifAbsent:			[^self error: 'Handle unknown transaction'].	self isError		ifTrue: [promise error: self error]		ifFalse: [promise fulfill: response].	aConnection objectCache reset	! !

!RsrDeliverResponse methodsFor!
response: anObject	response := anObject! !

!RsrDeliverResponse methodsFor!
transaction	^transaction! !

!RsrDeliverResponse methodsFor!
isError	^errorName notNil! !

!RsrDeliverResponse methodsFor!
errorName	^errorName! !

!RsrDeliverResponse methodsFor!
reportOn: aLog	aLog debug: 'RsrDeliverResponse/', self response class name! !

!RsrDeliverResponse methodsFor!
sendOver: aConnection	| analysis |	analysis := RsrRetainAnalysis		roots: roots		connection: aConnection.	analysis perform.	retainList := analysis retainCommands.	self encodeUsing: aConnection encoder.	aConnection commandWriter enqueue: self! !

!RsrDeliverResponse methodsFor!
error	^(RsrClassResolver classNamed: errorName ifAbsent: [RsrError]) new		messageText: response;		yourself! !

!RsrDeliverResponse methodsFor!
transaction: aTransactionId	transaction := aTransactionId! !

!RsrDeliverResponse methodsFor!
roots	^roots! !

!RsrEncoder methodsFor!
encodeObject: anObject	^ByteArray		streamContents:			[:stream |			self				encodeObject: anObject				onto: stream]! !

!RsrEncoder methodsFor!
encodeReleaseObjects: aReleaseObject	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self releaseObjectsCommand				onto: stream.			self				encodeControlWord: aReleaseObject oids size				onto: stream.			aReleaseObject oids				do:					[:oid |					self						encodeControlWord: oid						onto: stream]]! !

!RsrEncoder methodsFor!
isService: anObject	^anObject isKindOf: RsrService! !

!RsrEncoder methodsFor!
encodeControlWord: anIntegeronto: aStream	| encodedInteger encodedBytes |	(anInteger between: self controlWordMin and: self controlWordMax)		ifFalse: [self error: anInteger printString, ' is outside the supported size of a control word.'].	encodedInteger := (anInteger positive		ifTrue: [anInteger]		ifFalse: [(2 raisedTo: 64) + anInteger]).	encodedBytes := self		integerAsByteArray: encodedInteger		ofSize: self sizeOfInteger.	aStream nextPutAll: encodedBytes! !

!RsrEncoder methodsFor!
encodeRetainObject: aRetainObject	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self retainObjectIdentifier				onto: stream.			self				encodeObject: aRetainObject object				onto: stream]! !

!RsrEncoder methodsFor!
encodeReferenceOf: anObjectonto: aStream	| species |	species := self speciesOf: anObject.	species		encodeReference: anObject		using: self		onto: aStream! !

!RsrEncoder methodsFor!
retainObjectIdentifier	^0! !

!RsrEncoder methodsFor!
isImmediate: anObject	^self speciesMapping includesKey: anObject class! !

!RsrEncoder methodsFor!
encodeDeliverResponse: aDeliverResponse	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self deliverResponseCommand				onto: stream.			self				encodeControlWord: aDeliverResponse transaction				onto: stream.			self				encodeReferenceOf: aDeliverResponse errorName				onto: stream.			self				encodeReferenceOf: aDeliverResponse response				onto: stream]! !

!RsrEncoder methodsFor!
encodeSendMessage: aSendMessage	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self sendMessageIdentifier				onto: stream.			self				encodeControlWord: aSendMessage transaction				onto: stream.			self				encodeControlWord: aSendMessage arguments size				onto: stream.			self				encodeReferenceOf: aSendMessage receiver				onto: stream.			self				encodeReferenceOf: aSendMessage selector				onto: stream.			aSendMessage arguments				do:					[:each |					self						encodeReferenceOf: each						onto: stream]]! !

!RsrEncoder methodsFor!
sendMessageIdentifier	^1! !

!RsrEncoder methodsFor!
speciesOf: anObject	(self isService: anObject)		ifTrue: [^RsrServiceSpecies].	anObject == true		ifTrue: [^RsrTrueSpecies].	anObject == false		ifTrue: [^RsrFalseSpecies].	(anObject isKindOf: Integer)		ifTrue: [^anObject positive ifTrue: [RsrPositiveIntegerSpecies] ifFalse: [RsrNegativeIntegerSpecies]].	^self speciesMapping		at: anObject class		ifAbsent: [RsrSpecies nullSpecies]! !

!RsrEncoder methodsFor!
speciesMapping	"Return a mapping between the native class and their associated RsrSpecies"	^RsrSpecies speciesMapping! !

!RsrEncoder methodsFor!
encodeObject: anObjectonto: aStream	(self speciesOf: anObject)		encode: anObject		using: self		on: aStream! !

!RsrEncoder methodsFor!
integerAsByteArray: anIntegerofSize: aNumberOfBytes	| bytes int |	bytes := ByteArray new: aNumberOfBytes.	int := anInteger.	aNumberOfBytes		to: 1		by: -1		do:			[:i | | byte |			byte := int bitAnd: 16rFF.			int := int bitShift: -8.			bytes at: i put: byte].	int ~= 0		ifTrue: [self error: 'Loss of precision detected'].	^bytes! !

!RsrCommandSink methodsFor!
write: aByteArray	self stream nextPutAll: aByteArray! !

!RsrCommandSink methodsFor!
writeCommand: aCommand	self report: aCommand.	aCommand writeUsing: self! !

!RsrCommandSink methodsFor!
initialize	super initialize.	queue := SharedQueue new! !

!RsrCommandSink methodsFor!
stopToken	^self stoppedState! !

!RsrCommandSink methodsFor!
flush	self stream flush! !

!RsrCommandSink methodsFor!
stop	super stop.	queue nextPut: self stopToken! !

!RsrCommandSink methodsFor!
enqueue: aCommand	self isActive ifTrue: [queue nextPut: aCommand]! !

!RsrCommandSink methodsFor!
executeCycle	[| command |	command := queue next.	command == self stopToken		ifTrue: [^self].	self writeCommand: command.	(queue size = 0)		ifTrue: [self flush]]		on: RsrSocketClosed		do:			[:ex |			self reportException: ex.			self connection disconnected]! !

!RsrServiceFactory methodsFor!
serviceFor: aResponsibility	^remoteSelf create: aResponsibility! !

!RsrReleaseObjects methodsFor!
reportOn: aLog	aLog debug: 'RsrReleaseObjects/', self oids printString! !

!RsrReleaseObjects methodsFor!
executeFor: aConnection	| registry |	registry := aConnection registry.	oids do: [:oid | registry cleanupEntryFor: oid]! !

!RsrReleaseObjects methodsFor!
oids	^oids! !

!RsrReleaseObjects methodsFor!
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeReleaseObjects: self! !

!RsrReleaseObjects methodsFor!
oids: anArray	oids := anArray! !

!RsrRetainObject methodsFor!
reportOn: aLog	aLog debug: 'RsrRetainObject/', self object class name! !

!RsrRetainObject methodsFor!
= anEncodedObject	self == anEncodedObject		ifTrue: [^true].	self class == anEncodedObject class		ifFalse: [^false].	^self object = anEncodedObject object		and: [self encoding = anEncodedObject encoding]! !

!RsrRetainObject methodsFor!
object: anObject	object := anObject! !

!RsrRetainObject methodsFor!
executeFor: aConnection	aConnection objectCache add: object! !

!RsrRetainObject methodsFor!
writeUsing: aCommandWriter	super writeUsing: aCommandWriter! !

!RsrRetainObject methodsFor!
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeRetainObject: self! !

!RsrRetainObject methodsFor!
object	^ object! !

!RsrRetainObject methodsFor!
hash	^self object hash! !

!RsrService methodsFor!
_reflectedVariablesDo: aBlock	self _reflectedVariableIndecesDo: [:index | aBlock value: (self instVarAt: index)]! !

!RsrService methodsFor!
isClient	^self class isClientClass! !

!RsrService methodsFor!
_id	^_id! !

!RsrService methodsFor!
_reflectedVariableIndecesDo: aBlock	| allVariables |	allVariables := self class allInstVarNames.	self class _variablesToReflect		do:			[:varName | | index |			index := allVariables indexOf: varName.			aBlock value: index]! !

!RsrService methodsFor!
_id: anRsrIdconnection: aConnection	_id := anRsrId.	_connection := aConnection.	remoteSelf := aConnection _forwarderClass on: self! !

!RsrService methodsFor!
_variablesToReflect	^self class _variablesToReflect! !

!RsrService methodsFor!
remoteServiceName	^self isClient		ifTrue: [self class serverClassName]		ifFalse: [self class clientClassName]! !

!RsrService methodsFor!
_synchronize	"Return self to synchronize with the remote peer"	^self! !

!RsrService methodsFor!
serviceName	^self class name! !

!RsrService methodsFor!
isMirrored	^_connection ~~ nil! !

!RsrService methodsFor!
_addTo: aRegistry	aRegistry		serviceAt: _id		put: self! !

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
encodeUsing: anEncoder	encoding := anEncoder encodeSendMessage: self! !

!RsrSendMessage methodsFor!
arguments: anObject	arguments := anObject! !

!RsrSendMessage methodsFor!
executeFor: aConnection	self primExecuteFor: aConnection! !

!RsrSendMessage methodsFor!
selector	^ selector! !

!RsrSendMessage methodsFor!
transaction	^ transaction! !

!RsrSendMessage methodsFor!
selector: anObject	selector := anObject! !

!RsrSendMessage methodsFor!
receiver: anObject	receiver := anObject! !

!RsrSendMessage methodsFor!
reportOn: aLog	aLog debug: 'RsrSendMessage/', self receiver class name, '>>', self selector! !

!RsrSendMessage methodsFor!
roots	^(Array with: receiver with: selector) ,  arguments! !

!RsrSendMessage methodsFor!
transaction: anObject	transaction := anObject! !

!RsrSendMessage methodsFor!
sendOver: aConnection	| analysis promise |	analysis := RsrRetainAnalysis		roots: self roots		connection: aConnection.	analysis perform.	retainList := analysis retainCommands.	self encodeUsing: aConnection encoder.	promise := RsrPromise new.	aConnection promises		at: transaction		put: promise.	aConnection commandWriter enqueue: self.	^promise! !

!RsrSendMessage methodsFor!
primExecuteFor: aConnection	| result response |	[result := receiver		perform: selector		withArguments: arguments.	aConnection objectCache reset.	response := RsrDeliverResponse		transaction: transaction		response: result		roots: (Array with: receiver with: result).	response sendOver: aConnection]		on: Error		do: [:ex | (RsrDeliverResponse transaction: transaction error: ex roots: (Array with: receiver)) sendOver: aConnection]! !

!RsrServiceFactoryServer methodsFor!
create: aResponsibility	| abstractClass |	abstractClass := RsrClassResolver classNamed: aResponsibility.	^abstractClass serverClass new! !

!RsrEventLoop methodsFor!
stop	self isActive ifFalse: [^self].	state := self stoppedState.	self connection close.	self stream close! !

!RsrEventLoop methodsFor!
log: aString	self log debug: aString! !

!RsrEventLoop methodsFor!
executeCycle	self subclassResponsibility! !

!RsrEventLoop methodsFor!
start	state := self runningState.	process := RsrConcurrency		fork: [self runLoop.				process := nil]		at: self priority! !

!RsrEventLoop methodsFor!
isProcessActive	^process ~~ nil! !

!RsrEventLoop methodsFor!
stoppedState	^#Stop! !

!RsrEventLoop methodsFor!
priority	^Processor lowIOPriority! !

!RsrEventLoop methodsFor!
initialize	super initialize.	state := self stoppedState! !

!RsrEventLoop methodsFor!
runningState	^#Running! !

!RsrEventLoop methodsFor!
connection: anObject	connection := anObject! !

!RsrEventLoop methodsFor!
report: aCommand	aCommand reportOn: self log! !

!RsrEventLoop methodsFor!
runLoop	[self isActive]		whileTrue:			[[self executeCycle]				on: Error				do:					[:ex |					self reportException: ex.					self connection unknownError: ex]]! !

!RsrEventLoop methodsFor!
isActive	^state == self runningState! !

!RsrEventLoop methodsFor!
log	^RsrLogWithPrefix		prefix: self class name asString		log: self connection log! !

!RsrEventLoop methodsFor!
stream	^self connection stream! !

!RsrEventLoop methodsFor!
connection	^connection! !

!RsrEventLoop methodsFor!
reportException: anException	self log: '', self class name, '/', anException description! !

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

!RsrTranscriptSink methodsFor!
write: aMessageString	Transcript		show: aMessageString;		cr! !

!RsrStream methodsFor!
nextPutAll: aByteArray	^stream nextPutAll: aByteArray! !

!RsrStream methodsFor!
close	stream close! !

!RsrStream methodsFor!
next: aLength	| bytes |	bytes := stream next: aLength.	bytes size ~~ aLength		ifTrue: [RsrSocketClosed signal].	^bytes! !

!RsrStream methodsFor!
flush	stream flush! !

!RsrStream methodsFor!
next	^self next: 1! !

!RsrStream methodsFor!
stream: aStream	stream := aStream! !

!RsrStream methodsFor!
binary	stream binary! !

!RsrLog methodsFor!
error: aString	self verbosity >= self levelError		ifTrue: [self log: aString level: #error]! !

!RsrLog methodsFor!
levelInfo	^3! !

!RsrLog methodsFor!
debug: aString	self verbosity >= self levelDebug		ifTrue: [	self log: aString level: #debug]! !

!RsrLog methodsFor!
levelTrace	^5! !

!RsrLog methodsFor!
log: aMessagelevel: aLevelString	| message |	message := RsrDateAndTimeSpecies now printString, '-', aLevelString, '-', aMessage.	sinks do: [:each | each write: message]! !

!RsrLog methodsFor!
verbosity	^verbosity! !

!RsrLog methodsFor!
levelDebug	^4! !

!RsrLog methodsFor!
initialize	super initialize.	verbosity := self levelTrace.	sinks := OrderedCollection new! !

!RsrLog methodsFor!
warning: aString	self verbosity >= self levelDebug		ifTrue: [self log: aString level: #warning]! !

!RsrLog methodsFor!
verbosity: aLogLevel	verbosity := aLogLevel! !

!RsrLog methodsFor!
addSink: aLogSink	sinks add: aLogSink! !

!RsrLog methodsFor!
info: aString	self verbosity >= self levelInfo		ifTrue: [self log: aString level: #info]! !

!RsrLog methodsFor!
trace: aString	self verbosity >= self levelTrace		ifTrue: [self log: aString level: #trace]! !

!RsrLog methodsFor!
levelWarn	^2! !

!RsrLog methodsFor!
levelError	^1! !

!RsrCycleDetected methodsFor!
object: anObject	object := anObject! !

!RsrCycleDetected methodsFor!
messageText	^'Cycle detected on: ', object printString! !

!RsrCommandSource methodsFor!
decoder	^self connection decoder! !

!RsrCommandSource methodsFor!
nextCommand	^self decoder decodeCommand: self stream! !

!RsrCommandSource methodsFor!
dispatcher	^self connection dispatcher! !

!RsrCommandSource methodsFor!
executeCycle	[self dispatcher dispatch: self nextCommand]		on: RsrSocketClosed		do:			[:ex |			self reportException: ex.			self connection disconnected]! !

!RsrRetainAnalysis methodsFor!
perform	roots do: [:each | self analyze: each]! !

!RsrRetainAnalysis methodsFor!
initialize	super initialize.	retainCommands := OrderedCollection new.	inFlight := IdentitySet new! !

!RsrRetainAnalysis methodsFor!
roots: anObject	roots := anObject! !

!RsrRetainAnalysis methodsFor!
analyze: anObject	^(self speciesOf: anObject)		analyze: anObject		using: self! !

!RsrRetainAnalysis methodsFor!
nextOid	^self connection oidSpigot next! !

!RsrRetainAnalysis methodsFor!
registry	^connection registry! !

!RsrRetainAnalysis methodsFor!
analyzing: anObjectduring: aBlock	(inFlight includes: anObject)		ifTrue: [^RsrCycleDetected signal: anObject].	inFlight add: anObject.	aBlock value.	inFlight remove: anObject! !

!RsrRetainAnalysis methodsFor!
retainCommands	^retainCommands! !

!RsrRetainAnalysis methodsFor!
ensureRegistered: anRsrObject	anRsrObject isMirrored		ifTrue: [^self].	anRsrObject		_id: self nextOid		connection: self connection.	anRsrObject _addTo: self registry! !

!RsrRetainAnalysis methodsFor!
analyzeImmediate: anObject	"Nothing to do for a generic immediate"	^anObject! !

!RsrRetainAnalysis methodsFor!
encoder	^self connection encoder! !

!RsrRetainAnalysis methodsFor!
connection: aConnection	connection := aConnection! !

!RsrRetainAnalysis methodsFor!
speciesOf: anObject	^self encoder speciesOf: anObject! !

!RsrRetainAnalysis methodsFor!
roots	^roots! !

!RsrRetainAnalysis methodsFor!
analyzeDictionary: aDictionary	self		analyzing: aDictionary		during:			[aDictionary				keysAndValuesDo:					[:key :value |					self						analyze: key;						analyze: value]].	^aDictionary! !

!RsrRetainAnalysis methodsFor!
connection	^connection! !

!RsrRetainAnalysis methodsFor!
analyzeCollection: aCollection	self		analyzing: aCollection		during: [	aCollection do: [:each | self analyze: each]].	^aCollection! !

!RsrRetainAnalysis methodsFor!
analyzeService: aService	self ensureRegistered: aService.	self		analyzing: aService		during: [aService _reflectedVariablesDo: [:each | self analyze: each]].	self retain: aService! !

!RsrRetainAnalysis methodsFor!
retain: aService	| retainCommand |	retainCommand := RsrRetainObject object: aService.	retainCommand encodeUsing: self encoder.	self retainCommands add: retainCommand! !

!RsrLogSink methodsFor!
write: aMessage	self subclassResponsibility! !

!RsrConnection methodsFor!
serviceFor: aResponsibility	^self serviceFactory serviceFor: aResponsibility! !

!RsrConnection methodsFor!
close	isOpen		ifFalse: [^self].	isOpen := false.	commandReader stop.	commandWriter stop.	dispatcher stop.	self promises do: [:each | each error: RsrConnectionClosed new].	objectCache reset.	closeSemaphore signal! !

!RsrConnection methodsFor!
log	^log! !

!RsrConnection methodsFor!
decoder	^RsrDecoder registry: registry connection: self! !

!RsrConnection methodsFor!
initialize	super initialize.	isOpen := false.	transactionSpigot := RsrThreadSafeNumericSpigot naturals.	objectCache := RsrObjectCache new.	promises := Dictionary new.	registry := RsrRegistry reapAction: [:oid | self releaseOid: oid].	log := RsrLog new! !

!RsrConnection methodsFor!
transactionSpigot	^transactionSpigot! !

!RsrConnection methodsFor!
stream	^stream! !

!RsrConnection methodsFor!
isClosed	^self isOpen not! !

!RsrConnection methodsFor!
oidSpigot	^oidSpigot! !

!RsrConnection methodsFor!
objectCache	^objectCache! !

!RsrConnection methodsFor!
oidSpigot: anIntegerSpigot	oidSpigot := anIntegerSpigot! !

!RsrConnection methodsFor!
disconnected	self log info: 'Disconnected'.	self close! !

!RsrConnection methodsFor!
dispatcher	^dispatcher! !

!RsrConnection methodsFor!
newTransactionId	^transactionSpigot next! !

!RsrConnection methodsFor!
promises	^promises! !

!RsrConnection methodsFor!
_sendMessage: aMessageto: aService"Open coordination window"	"Send dirty transitive closure of aRemoteMessage"	"Send DispatchMessage command""Coorination window closed"	"Return Promise"	| dispatchCommand |	isOpen		ifFalse: [self error: 'Connection is not open'].	dispatchCommand := RsrSendMessage		transaction: self newTransactionId		receiver: aService		selector: aMessage selector		arguments: aMessage arguments.	^dispatchCommand sendOver: self! !

!RsrConnection methodsFor!
commandReader	^commandReader! !

!RsrConnection methodsFor!
registry	^registry! !

!RsrConnection methodsFor!
commandWriter	^commandWriter! !

!RsrConnection methodsFor!
unknownError: anException	self close! !

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
transactionSpigot: anObject	transactionSpigot := anObject! !

!RsrConnection methodsFor!
open	(isOpen := socket isConnected)		ifFalse: [^RsrConnectionClosed signal].	closeSemaphore := Semaphore new.	stream := RsrSocketStream on: socket.	dispatcher := RsrDispatchEventLoop on: self.	commandReader := RsrCommandSource on: self.	commandWriter := RsrCommandSink on: self.	dispatcher start.	commandReader start.	commandWriter start.	serviceFactory := RsrServiceFactory		_id: self oidSpigot next		connection: self.	serviceFactory _addTo: registry! !

!RsrConnection methodsFor!
isOpen	^isOpen! !

!RsrConnection methodsFor!
socket: aSocket	socket := aSocket! !

!RsrThreadSafeNumericSpigot methodsFor!
initialize	super initialize.	mutex := Semaphore forMutualExclusion! !

!RsrThreadSafeNumericSpigot methodsFor!
next	^mutex critical: [super next]! !

!RsrCodec methodsFor!
sendMessageCommand	^1! !

!RsrCodec methodsFor!
deliverResponseCommand	^2! !

!RsrCodec methodsFor!
controlWordMin	^(2 raisedTo: 63) negated! !

!RsrCodec methodsFor!
controlWordMax	^(2 raisedTo: 63) - 1! !

!RsrCodec methodsFor!
immediateOID	^0! !

!RsrCodec methodsFor!
releaseObjectsCommand	^3! !

!RsrCodec methodsFor!
sizeOfInteger	"Return the number of bytes used to encode an integer"	^8! !

!RsrCodec methodsFor!
retainObjectCommand	^0! !

!RsrDecoder methodsFor!
initializeDecodeCommandMap	decodeCommandMap := Dictionary new.	decodeCommandMap		at: self retainObjectCommand put: #decodeRetainObject:;		at: self sendMessageCommand put: #decodeSendMessage:;		at: self deliverResponseCommand put: #decodeDeliverResponse:;		at: self releaseObjectsCommand put: #decodeReleaseObjects:.	^decodeCommandMap! !

!RsrDecoder methodsFor!
connection	^connection! !

!RsrDecoder methodsFor!
decodeCommandMap	^decodeCommandMap ifNil: [self initializeDecodeCommandMap]! !

!RsrDecoder methodsFor!
decodeControlWord: aStream	| bytes unsignedResult |	bytes := aStream next: self sizeOfInteger.	unsignedResult := self bytesAsInteger: bytes.	^unsignedResult > self controlWordMax		ifTrue: [(2 raisedTo: 64) negated + unsignedResult]		ifFalse: [unsignedResult]! !

!RsrDecoder methodsFor!
registry: anRsrRegistry	registry := anRsrRegistry! !

!RsrDecoder methodsFor!
decodeReleaseObjects: aStream	| count oids |	count := self decodeControlWord: aStream.	oids := Array new: count.	1		to: count		do:			[:i | | oid |			oid := self decodeControlWord: aStream.			oids at: i put: oid].	^RsrReleaseObjects oids: oids! !

!RsrDecoder methodsFor!
registry	^registry! !

!RsrDecoder methodsFor!
signalUnknownOID	RsrUnknownOID signal! !

!RsrDecoder methodsFor!
decodeImmediateObject: aStream	| species |	species := self decodeControlWord: aStream.	^(RsrSpecies speciesList at: species + 1)		decodeReference: aStream		using: self! !

!RsrDecoder methodsFor!
decodeDeliverResponse: aStream	| transaction errorName response |	transaction := self decodeControlWord: aStream.	errorName := self decodeObjectReference: aStream.	response := self decodeObjectReference: aStream.	^RsrDeliverResponse new		transaction: transaction;		errorName: errorName;		response: response;		yourself! !

!RsrDecoder methodsFor!
connection: aConnection	connection := aConnection! !

!RsrDecoder methodsFor!
decodeObjectReference: aStream	| oid |	oid := self decodeControlWord: aStream.	oid = self immediateOID ifTrue: [^self decodeImmediateObject: aStream].	^registry serviceAt: oid ifAbsent: [self signalUnknownOID]! !

!RsrDecoder methodsFor!
decodeSendMessage: aStream	| transaction argCount receiverOID receiver selector arguments |	transaction := self decodeControlWord: aStream.	argCount := self decodeControlWord: aStream.	receiverOID := self decodeControlWord: aStream.	receiver := registry serviceAt: receiverOID ifAbsent: [^self signalUnknownOID].	selector := self decodeObjectReference: aStream.	arguments := (1 to: argCount) collect: [:each | self decodeObjectReference: aStream].	^RsrSendMessage		transaction: transaction		receiver: receiver		selector: selector		arguments: arguments! !

!RsrDecoder methodsFor!
lookupClass: aClassName	^RsrClassResolver classNamed: aClassName! !

!RsrDecoder methodsFor!
decodeCommand: aStream	"Decode an object from the stream"	| command decodeSelector |	command := self decodeControlWord: aStream.	decodeSelector := self decodeCommandMap		at: command		ifAbsent: [self error: 'Invalid command received'].	^self		perform: decodeSelector		with: aStream! !

!RsrDecoder methodsFor!
bytesAsInteger: bytes	| res |	res := 0.	bytes do: [:e | res := (res bitShift: 8) bitOr: e].	^res! !

!RsrDecoder methodsFor!
decodeRetainObject: aStream	^RsrRetainObject object: (self decodeService: aStream)! !

!RsrDecoder methodsFor!
decodeService: aStream	^RsrServiceSpecies		decode: aStream		using: self! !

!RsrSocketStream methodsFor!
isConnected	^socket isConnected! !

!RsrSocketStream methodsFor!
socket: anRsrSocket	socket := anRsrSocket! !

!RsrSocketStream methodsFor!
nextPutAll: bytes	socket write: bytes! !

!RsrSocketStream methodsFor!
flush	"NOP"! !

!RsrSocketStream methodsFor!
next: aCount	aCount = 0		ifTrue: [^#[]].	^socket read: aCount! !

!RsrSocketStream methodsFor!
next	^self next: 1! !

!RsrSocketStream methodsFor!
close	socket close! !

!RsrSocketStream methodsFor!
atEnd	^self isConnected! !

!RsrCommand methodsFor!
reportOn: aLog	self subclassResponsibility! !

!RsrCommand methodsFor!
writeUsing: aCommandWriter	aCommandWriter write: encoding! !

!RsrCommand methodsFor!
executeFor: aConnection	self subclassResponsibility! !

!RsrCommand methodsFor!
sendOver: aConnection	"Do nothing unless a subclass offers a specialization"! !

!RsrCommand methodsFor!
encoding	^ encoding! !

!RsrCommand methodsFor!
encodeUsing: anRsrEncoder	self subclassResponsibility! !

!RsrCommand methodsFor!
encoding: anObject	encoding := anObject! !

!RsrDispatchEventLoop methodsFor!
initialize	super initialize.	queue := SharedQueue new! !

!RsrDispatchEventLoop methodsFor!
stopToken	^self stoppedState! !

!RsrDispatchEventLoop methodsFor!
priority	^super priority - 1! !

!RsrDispatchEventLoop methodsFor!
stop	super stop.	queue nextPut: self stopToken! !

!RsrDispatchEventLoop methodsFor!
dispatch: aCommand	self report: aCommand.	queue nextPut: aCommand! !

!RsrDispatchEventLoop methodsFor!
executeCycle	| item |	item := queue next.	item == self stopToken		ifFalse: [item executeFor: connection]! !