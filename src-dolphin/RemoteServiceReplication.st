RsrProtoObject
	subclass: #RsrForwarder
	instanceVariableNames: 'rsrObject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrForwarder categoriesForClass!RemoteServiceReplication-Utilities! !

RsrObject
	subclass: #RsrBufferedSocketStream
	instanceVariableNames: 'stream outBuffer writePosition nextToWrite'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrBufferedSocketStream categoriesForClass!RemoteServiceReplication-Utilities! !

RsrObject
	subclass: #RsrCodec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCodec categoriesForClass!RemoteServiceReplication-Communications! !

RsrObject
	subclass: #RsrCommand
	instanceVariableNames: 'encoding'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCommand categoriesForClass!RemoteServiceReplication-Communications! !

RsrObject
	subclass: #RsrCommandProcessor
	instanceVariableNames: 'process connection state'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCommandProcessor categoriesForClass!RemoteServiceReplication-Communications! !

RsrObject
	subclass: #RsrConnection
	instanceVariableNames: 'isOpen transactionSpigot commandWriter commandReader registry objectCache socket stream promises dispatcher oidSpigot serviceFactory log'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrConnection categoriesForClass!RemoteServiceReplication-Communications! !

RsrObject
	subclass: #RsrLog
	instanceVariableNames: 'verbosity'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrLog categoriesForClass!RemoteServiceReplication-Debugging! !

RsrObject
	subclass: #RsrNumericSpigot
	instanceVariableNames: 'current step'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrNumericSpigot categoriesForClass!RemoteServiceReplication-Utilities! !

RsrObject
	subclass: #RsrObjectCache
	instanceVariableNames: 'storage'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrObjectCache categoriesForClass!RemoteServiceReplication-Utilities! !

RsrObject
	subclass: #RsrPromise
	instanceVariableNames: 'mutex value error markerValue'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrPromise categoriesForClass!RemoteServiceReplication-Utilities! !

RsrObject
	subclass: #RsrRetainAnalysis
	instanceVariableNames: 'roots retainCommands inFlight connection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRetainAnalysis categoriesForClass!RemoteServiceReplication-Communications! !

RsrAbstractService
	subclass: #RsrService
	instanceVariableNames: 'rsrId rsrConnection remoteSelf'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrService comment: 'I represent a class of Objects that know offer Rsr Services.'!
!RsrService categoriesForClass!RemoteServiceReplication-Public! !

RsrObject
	subclass: #RsrSocketStream
	instanceVariableNames: 'socket'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSocketStream categoriesForClass!RemoteServiceReplication-Utilities! !

RsrObject
	subclass: #RsrStream
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrStream categoriesForClass!RemoteServiceReplication-Communications! !

RsrService
	subclass: #RsrAbstractServiceFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrAbstractServiceFactory categoriesForClass!RemoteServiceReplication-Public-Examples! !

RsrCommandProcessor
	subclass: #RsrCommandDispatcher
	instanceVariableNames: 'queue'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCommandDispatcher categoriesForClass!RemoteServiceReplication-Communications! !

RsrCommandProcessor
	subclass: #RsrCommandReader
	instanceVariableNames: 'decoder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCommandReader categoriesForClass!RemoteServiceReplication-Communications! !

RsrCommandProcessor
	subclass: #RsrCommandWriter
	instanceVariableNames: 'queue'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCommandWriter categoriesForClass!RemoteServiceReplication-Communications! !

RsrCodec
	subclass: #RsrDecoder
	instanceVariableNames: 'registry connection decodeCommandMap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDecoder categoriesForClass!RemoteServiceReplication-Communications! !

RsrCommand
	subclass: #RsrDeliverResponse
	instanceVariableNames: 'transaction errorName response retainList'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDeliverResponse categoriesForClass!RemoteServiceReplication-Communications! !

RsrCodec
	subclass: #RsrEncoder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrEncoder categoriesForClass!RemoteServiceReplication-Communications! !

RsrCommand
	subclass: #RsrReleaseObjects
	instanceVariableNames: 'oids'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrReleaseObjects categoriesForClass!RemoteServiceReplication-Communications! !

RsrCommand
	subclass: #RsrRetainObject
	instanceVariableNames: 'object'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRetainObject categoriesForClass!RemoteServiceReplication-Communications! !

RsrCommand
	subclass: #RsrSendMessage
	instanceVariableNames: 'transaction receiver selector arguments retainList'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSendMessage categoriesForClass!RemoteServiceReplication-Communications! !

RsrNumericSpigot
	subclass: #RsrThreadSafeNumericSpigot
	instanceVariableNames: 'mutex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrThreadSafeNumericSpigot categoriesForClass!RemoteServiceReplication-Utilities! !

RsrError
	subclass: #RsrCycleDetected
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCycleDetected categoriesForClass!RemoteServiceReplication-Exceptions! !

RsrAbstractServiceFactory
	subclass: #RsrServiceFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceFactory categoriesForClass!RemoteServiceReplication-Public-Examples! !

RsrAbstractServiceFactory
	subclass: #RsrServiceFactoryServer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceFactoryServer categoriesForClass!RemoteServiceReplication-Public-Examples! !

RsrError
	subclass: #RsrUnknownOID
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrUnknownOID categoriesForClass!RemoteServiceReplication-Exceptions! !

!RsrService class methodsFor!
_id: anIdconnection: aConnection	^super new		_id: anId connection: aConnection;		yourself! !

!RsrService class methodsFor!
clientClass	self subclassResponsibility! !

!RsrService class methodsFor!
serverClass	self subclassResponsibility! !

!RsrService class methodsFor!
isClientClass	^self == self clientClass! !

!RsrService class methodsFor!
isServerClass	^self == self serverClass! !

!RsrService class methodsFor!
variablesToReflect	| currentClass variables |	variables := OrderedCollection new.	currentClass := self superclass.	[currentClass == RsrService]		whileFalse:			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].			currentClass := currentClass superclass].	^variables! !

!RsrDeliverResponse class methodsFor!
transaction: aTransactionIdresponse: anObject	^self new		transaction: aTransactionId;		response: anObject;		yourself! !

!RsrDeliverResponse class methodsFor!
transaction: aTransactionIderror: anException	^self new		transaction: aTransactionId;		errorName: anException class name;		response: anException messageText;		yourself! !

!RsrBufferedSocketStream class methodsFor!
on: aSocketStream	^self new		stream: aSocketStream;		yourself! !

!RsrRetainObject class methodsFor!
object: anRsrObject	^self new		object: anRsrObject;		yourself! !

!RsrRetainObject class methodsFor!
object: anRsrObjectencoding: aByteArray	^self new		object: anRsrObject;		encoding: aByteArray;		yourself! !

!RsrDecoder class methodsFor!
registry: anRsrRegistryconnection: aConnection	^self new		registry: anRsrRegistry;		connection: aConnection;		yourself! !

!RsrRetainAnalysis class methodsFor!
roots: anArrayconnection: aConnection	^self new		roots: anArray;		connection: aConnection;		yourself! !

!RsrSendMessage class methodsFor!
transaction: aTransactionIdreceiver: aServiceselector: aSelectorarguments: anArray	^self new		transaction: aTransactionId;		receiver: aService;		selector: aSelector;		arguments: anArray;		yourself! !

!RsrForwarder class methodsFor!
on: anRsrObject	| instance |	instance := self new.	instance _object: anRsrObject.	^instance! !

!RsrReleaseObjects class methodsFor!
oids: anArray	^self new		oids: anArray;		yourself! !

!RsrAbstractServiceFactory class methodsFor!
clientClass	^RsrServiceFactory! !

!RsrAbstractServiceFactory class methodsFor!
serverClass	^RsrServiceFactoryServer! !

!RsrConnection class methodsFor!
socket: aSockettransactionSpigot: aNumericSpigotoidSpigot: anOidSpigot	^super new		socket: aSocket;		transactionSpigot: aNumericSpigot;		oidSpigot: anOidSpigot;		yourself! !

!RsrConnection class methodsFor!
new	self error: 'Instance creation via #new is unsupported'! !

!RsrConnection class methodsFor!
connectTo: aPortNumberon: aHostname	| socket |	socket := RsrSocket new.	socket		connectTo: aPortNumber		on: aHostname.	^(self		socket: socket		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1) negated) open! !

!RsrConnection class methodsFor!
acceptOn: aPortNumber	| listener socket |	listener := RsrSocket new.	listener listenOn: aPortNumber.	socket := listener accept.	listener close.	^(self		socket: socket		transactionSpigot: RsrThreadSafeNumericSpigot naturals		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1)) open! !

!RsrConnection class methodsFor!
connectionTimeout	^2! !

!RsrSocketStream class methodsFor!
on: anRsrSocket	^self new		socket: anRsrSocket;		yourself! !

!RsrStream class methodsFor!
on: aStream	^self new		stream: aStream;		yourself! !

!RsrNumericSpigot class methodsFor!
new	^self		start: 0		step: 1! !

!RsrNumericSpigot class methodsFor!
naturals	^self		start: 1		step: 1! !

!RsrNumericSpigot class methodsFor!
start: aNumberstep: anIncrement	^super new		start: aNumber;		step: anIncrement;		yourself! !

!RsrCommandProcessor class methodsFor!
on: aConnection	^self new		connection: aConnection;		yourself! !

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

!RsrCommandReader methodsFor!
decoder	^self connection decoder! !

!RsrCommandReader methodsFor!
nextCommand	^self decoder decodeCommand: self stream! !

!RsrCommandReader methodsFor!
dispatcher	^self connection dispatcher! !

!RsrCommandReader methodsFor!
executeCycle	[self dispatcher dispatch: self nextCommand]		on: RsrConnectionClosed		do:			[:ex |			self log: ex.			self connection disconnected]! !

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

!RsrForwarder methodsFor!
doesNotUnderstand: aMessage	| promise |	promise := rsrObject _connection		_sendMessage: aMessage		to: rsrObject.	^promise value! !

!RsrForwarder methodsFor!
_object: anRsrObject	rsrObject := anRsrObject! !

!RsrDeliverResponse methodsFor!
response	^response! !

!RsrDeliverResponse methodsFor!
writeUsing: aCommandWriter	retainList do: [:each | each writeUsing: aCommandWriter].	aCommandWriter write: encoding! !

!RsrDeliverResponse methodsFor!
errorName: aSymbol	errorName := aSymbol! !

!RsrDeliverResponse methodsFor!
executeFor: aConnection	| promise |	promise := aConnection promises		removeKey: transaction		ifAbsent:			[^self error: 'Handle unknown transaction'].	self isError		ifTrue: [promise error: self error]		ifFalse: [promise fulfill: response].	aConnection objectCache reset	! !

!RsrDeliverResponse methodsFor!
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeDeliverResponse: self! !

!RsrDeliverResponse methodsFor!
response: anObject	response := anObject! !

!RsrDeliverResponse methodsFor!
isError	^errorName notNil! !

!RsrDeliverResponse methodsFor!
transaction	^transaction! !

!RsrDeliverResponse methodsFor!
errorName	^errorName! !

!RsrDeliverResponse methodsFor!
sendOver: aConnection	| analysis |	analysis := RsrRetainAnalysis		roots: (Array with: response)		connection: aConnection.	analysis perform.	retainList := analysis retainCommands.	self encodeUsing: aConnection encoder.	aConnection commandWriter enqueue: self! !

!RsrDeliverResponse methodsFor!
error	^(RsrClassResolver classNamed: errorName ifAbsent: [RsrError]) new		messageText: response;		yourself! !

!RsrDeliverResponse methodsFor!
transaction: aTransactionId	transaction := aTransactionId! !

!RsrEncoder methodsFor!
encodeObject: anObject	^ByteArray		streamContents:			[:stream |			self				encodeObject: anObject				onto: stream]! !

!RsrEncoder methodsFor!
isService: anObject	^anObject isKindOf: RsrService! !

!RsrEncoder methodsFor!
encodeRetainObject: aRetainObject	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self retainObjectIdentifier				onto: stream.			self				encodeObject: aRetainObject object				onto: stream]! !

!RsrEncoder methodsFor!
encodeObject: anObjectonto: aStream	(self speciesOf: anObject)		encode: anObject		using: self		on: aStream! !

!RsrEncoder methodsFor!
sendMessageIdentifier	^1! !

!RsrEncoder methodsFor!
integerAsByteArray: anIntegerofSize: aNumberOfBytes	| bytes int |	bytes := ByteArray new: aNumberOfBytes.	int := anInteger.	aNumberOfBytes		to: 1		by: -1		do:			[:i | | byte |			byte := int bitAnd: 16rFF.			int := int bitShift: -8.			bytes at: i put: byte].	int ~= 0		ifTrue: [self error: 'Loss of precision detected'].	^bytes! !

!RsrEncoder methodsFor!
encodeReferenceOf: anObjectonto: aStream	| species |	species := self speciesOf: anObject.	species		encodeReference: anObject		using: self		onto: aStream! !

!RsrEncoder methodsFor!
retainObjectIdentifier	^0! !

!RsrEncoder methodsFor!
encodeControlWord: anIntegeronto: aStream	| encodedInteger encodedBytes |	(anInteger between: self controlWordMin and: self controlWordMax)		ifFalse: [self error: anInteger printString, ' is outside the supported size of a control word.'].	encodedInteger := (anInteger positive		ifTrue: [anInteger]		ifFalse: [(2 raisedTo: 64) + anInteger]).	encodedBytes := self		integerAsByteArray: encodedInteger		ofSize: self sizeOfInteger.	aStream nextPutAll: encodedBytes! !

!RsrEncoder methodsFor!
encodeSendMessage: aSendMessage	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self sendMessageIdentifier				onto: stream.			self				encodeControlWord: aSendMessage transaction				onto: stream.			self				encodeControlWord: aSendMessage arguments size				onto: stream.			self				encodeReferenceOf: aSendMessage receiver				onto: stream.			self				encodeReferenceOf: aSendMessage selector				onto: stream.			aSendMessage arguments				do:					[:each |					self						encodeReferenceOf: each						onto: stream]]! !

!RsrEncoder methodsFor!
encodeDeliverResponse: aDeliverResponse	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self deliverResponseCommand				onto: stream.			self				encodeControlWord: aDeliverResponse transaction				onto: stream.			self				encodeReferenceOf: aDeliverResponse errorName				onto: stream.			self				encodeReferenceOf: aDeliverResponse response				onto: stream]! !

!RsrEncoder methodsFor!
speciesOf: anObject	(self isService: anObject)		ifTrue: [^RsrServiceSpecies].	anObject == true		ifTrue: [^RsrTrueSpecies].	anObject == false		ifTrue: [^RsrFalseSpecies].	(anObject isKindOf: Integer)		ifTrue: [^anObject positive ifTrue: [RsrPositiveIntegerSpecies] ifFalse: [RsrNegativeIntegerSpecies]].	^self speciesMapping		at: anObject class		ifAbsent: [RsrSpecies nullSpecies]! !

!RsrEncoder methodsFor!
speciesMapping	"Return a mapping between the native class and their associated RsrSpecies"	^RsrSpecies speciesMapping! !

!RsrEncoder methodsFor!
encodeReleaseObjects: aReleaseObject	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self releaseObjectsCommand				onto: stream.			self				encodeControlWord: aReleaseObject oids size				onto: stream.			aReleaseObject oids				do:					[:oid |					self						encodeControlWord: oid						onto: stream]]! !

!RsrEncoder methodsFor!
isImmediate: anObject	^self speciesMapping includesKey: anObject class! !

!RsrServiceFactory methodsFor!
serviceFor: aResponsibility	| instance |	instance := (RsrClassResolver classNamed: aResponsibility) basicNew.	self mirror: instance.	^instance! !

!RsrServiceFactory methodsFor!
mirror: aService	remoteSelf return: aService! !

!RsrReleaseObjects methodsFor!
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeReleaseObjects: self! !

!RsrReleaseObjects methodsFor!
executeFor: aConnection	| registry |	registry := aConnection registry.	oids do: [:oid | registry removeKey: oid]! !

!RsrReleaseObjects methodsFor!
oids	^oids! !

!RsrReleaseObjects methodsFor!
oids: anArray	oids := anArray! !

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
synchronize	remoteSelf == nil		ifFalse: [remoteSelf _synchronize]! !

!RsrService methodsFor!
_id	^rsrId! !

!RsrService methodsFor!
isNotMirrored	^self isMirrored not! !

!RsrService methodsFor!
remoteServiceName	^self remoteServiceClass name! !

!RsrService methodsFor!
isServer	^self class isServerClass! !

!RsrService methodsFor!
isClient	^self class isClientClass! !

!RsrService methodsFor!
variablesToReflect	^self class variablesToReflect! !

!RsrService methodsFor!
remoteServiceClass	^self isClient		ifTrue: [self class serverClass]		ifFalse: [self class clientClass]! !

!RsrService methodsFor!
_synchronize	"Return self to synchronize with the remote peer"	^self! !

!RsrService methodsFor!
serviceName	^self class name! !

!RsrService methodsFor!
_connection	^rsrConnection! !

!RsrService methodsFor!
_addTo: aRegistry	aRegistry		at: rsrId		put: self! !

!RsrService methodsFor!
_id: anRsrIdconnection: aConnection	rsrId := anRsrId.	rsrConnection := aConnection.	remoteSelf := aConnection _forwarderClass on: self! !

!RsrService methodsFor!
isMirrored	^rsrConnection ~~ nil! !

!RsrService methodsFor!
_reflectedVariableIndecesDo: aBlock	| allVariables |	allVariables := self class allInstVarNames.	self class variablesToReflect		do:			[:varName | | index |			index := allVariables indexOf: varName.			aBlock value: index]! !

!RsrService methodsFor!
_reflectedVariablesDo: aBlock	self _reflectedVariableIndecesDo: [:index | aBlock value: (self instVarAt: index)]! !

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
executeFor: aConnection	| result response |	[result := receiver		perform: selector		withArguments: arguments.	aConnection objectCache reset.	response := RsrDeliverResponse		transaction: transaction		response: result.	response sendOver: aConnection]		on: Error		do: [:ex | (RsrDeliverResponse transaction: transaction error: ex) sendOver: aConnection]! !

!RsrSendMessage methodsFor!
selector	^ selector! !

!RsrSendMessage methodsFor!
transaction	^ transaction! !

!RsrSendMessage methodsFor!
selector: anObject	selector := anObject! !

!RsrSendMessage methodsFor!
receiver: anObject	receiver := anObject! !

!RsrSendMessage methodsFor!
sendOver: aConnection	| analysis promise |	analysis := RsrRetainAnalysis		roots: self roots		connection: aConnection.	analysis perform.	retainList := analysis retainCommands.	self encodeUsing: aConnection encoder.	promise := RsrPromise new.	aConnection promises		at: transaction		put: promise.	aConnection commandWriter enqueue: self.	^promise! !

!RsrSendMessage methodsFor!
roots	^(Array with: receiver with: selector) ,  arguments! !

!RsrSendMessage methodsFor!
transaction: anObject	transaction := anObject! !

!RsrServiceFactoryServer methodsFor!
return: aService	^aService! !

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

!RsrObjectCache methodsFor!
initialize	super initialize.	self reset! !

!RsrObjectCache methodsFor!
add: anObject	storage add: anObject! !

!RsrObjectCache methodsFor!
reset	storage := IdentitySet new! !

!RsrStream methodsFor!
nextPutAll: aByteArray	^stream nextPutAll: aByteArray! !

!RsrStream methodsFor!
close	stream close! !

!RsrStream methodsFor!
next	^self next: 1! !

!RsrStream methodsFor!
next: aLength	| bytes |	bytes := stream next: aLength.	bytes size ~~ aLength		ifTrue: [RsrConnectionClosed signal].	^bytes! !

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
log: aString	Transcript		show: RsrDateAndTimeSpecies now printString, '-', aString;		cr! !

!RsrLog methodsFor!
levelTrace	^5! !

!RsrLog methodsFor!
debug: aString	self verbosity >= self levelDebug		ifTrue: [	self log: 'DEBUG: ', aString]! !

!RsrLog methodsFor!
levelDebug	^4! !

!RsrLog methodsFor!
log: aMessagelevel: aLevelString	Transcript		show: RsrDateAndTimeSpecies now printString, '-', aLevelString, '-', aMessage;		cr! !

!RsrLog methodsFor!
verbosity	^verbosity! !

!RsrLog methodsFor!
warn: aString	self verbosity >= self levelDebug		ifTrue: [self log: 'WARN: ', aString]! !

!RsrLog methodsFor!
initialize	super initialize.	verbosity := self levelTrace! !

!RsrLog methodsFor!
verbosity: aLogLevel	verbosity := aLogLevel! !

!RsrLog methodsFor!
info: aString	self verbosity >= self levelInfo		ifTrue: [self log: 'INFO: ', aString]! !

!RsrLog methodsFor!
levelWarn	^2! !

!RsrLog methodsFor!
trace: aString	self verbosity >= self levelTrace		ifTrue: [self log: 'TRACE: ', aString]! !

!RsrLog methodsFor!
error: aString	self verbosity >= self levelError		ifTrue: [self log: 'ERROR: ', aString]! !

!RsrRetainAnalysis methodsFor!
isImmediate: anObject	^self encoder isImmediate: anObject! !

!RsrRetainAnalysis methodsFor!
perform	roots do: [:each | self process: each]! !

!RsrRetainAnalysis methodsFor!
initialize	super initialize.	retainCommands := OrderedCollection new.	inFlight := IdentitySet new! !

!RsrRetainAnalysis methodsFor!
roots: anObject	roots := anObject! !

!RsrRetainAnalysis methodsFor!
process: anObject	(self isService: anObject)		ifTrue: [^self processService: anObject].	(self isCollection: anObject)		ifTrue: [^self processCollection: anObject].	(self isImmediate: anObject)		ifTrue: [^anObject].	^RsrUnsupportedObject signal: 'Unsupported object (' , anObject printString , ')'! !

!RsrRetainAnalysis methodsFor!
nextOid	^self connection oidSpigot next! !

!RsrRetainAnalysis methodsFor!
isService: anObject	^anObject isKindOf: RsrService! !

!RsrRetainAnalysis methodsFor!
registry	^connection registry! !

!RsrRetainAnalysis methodsFor!
processDictionary: aDictionary	aDictionary		keysAndValuesDo:			[:key :value |			self				process: key;				process: value].	^aDictionary! !

!RsrRetainAnalysis methodsFor!
ensureRegistered: anRsrObject	anRsrObject isMirrored		ifFalse:			[anRsrObject				_id: self nextOid				connection: self connection.			anRsrObject _addTo: self registry]! !

!RsrRetainAnalysis methodsFor!
processCollection: aCollection	(self isDictionary: aCollection)		ifTrue: [^self processDictionary: aCollection].	aCollection do: [:each | self process: each].	^aCollection! !

!RsrRetainAnalysis methodsFor!
retainCommands	^retainCommands! !

!RsrRetainAnalysis methodsFor!
encoder	^self connection encoder! !

!RsrRetainAnalysis methodsFor!
isDictionary: anObject	^anObject class == Dictionary! !

!RsrRetainAnalysis methodsFor!
isCollection: anObject	^anObject isKindOf: Collection! !

!RsrRetainAnalysis methodsFor!
connection: aConnection	connection := aConnection! !

!RsrRetainAnalysis methodsFor!
roots	^roots! !

!RsrRetainAnalysis methodsFor!
connection	^connection! !

!RsrRetainAnalysis methodsFor!
processService: aService	self		processing: aService		during: [aService _reflectedVariablesDo: [:each | self process: each]]! !

!RsrRetainAnalysis methodsFor!
processing: anObjectduring: aBlock	(inFlight includes: anObject)		ifTrue: [^RsrCycleDetected signal: 'Cycled detected on: ', anObject printString].	inFlight add: anObject.	self ensureRegistered: anObject.	aBlock value.	self retain: anObject.	inFlight remove: anObject! !

!RsrRetainAnalysis methodsFor!
retain: anRsrObject	| retainCommand |	retainCommand := RsrRetainObject object: anRsrObject.	retainCommand encodeUsing: self encoder.	self retainCommands add: retainCommand! !

!RsrConnection methodsFor!
serviceFor: aResponsibility	^serviceFactory serviceFor: aResponsibility! !

!RsrConnection methodsFor!
close	isOpen		ifFalse: [^self].	isOpen := false.	commandReader stop.	commandWriter stop.	dispatcher stop.	self promises do: [:each | each error: RsrConnectionClosed new].	objectCache reset! !

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
oidSpigot	^oidSpigot! !

!RsrConnection methodsFor!
objectCache	^objectCache! !

!RsrConnection methodsFor!
oidSpigot: anIntegerSpigot	oidSpigot := anIntegerSpigot! !

!RsrConnection methodsFor!
disconnected	self log info: 'Disconnected'.	self close! !

!RsrConnection methodsFor!
promises	^promises! !

!RsrConnection methodsFor!
newTransactionId	^transactionSpigot next! !

!RsrConnection methodsFor!
commandWriter	^commandWriter! !

!RsrConnection methodsFor!
commandReader	^commandReader! !

!RsrConnection methodsFor!
dispatcher	^dispatcher! !

!RsrConnection methodsFor!
registry	^registry! !

!RsrConnection methodsFor!
_sendMessage: aMessageto: aService"Open coordination window"	"Send dirty transitive closure of aRemoteMessage"	"Send DispatchMessage command""Coorination window closed"	"Return Promise"	| dispatchCommand |	isOpen		ifFalse: [self error: 'Connection is not open'].	dispatchCommand := RsrSendMessage		transaction: self newTransactionId		receiver: aService		selector: aMessage selector		arguments: aMessage arguments.	^dispatchCommand sendOver: self! !

!RsrConnection methodsFor!
unknownError: anException	self close! !

!RsrConnection methodsFor!
encoder	^RsrEncoder new! !

!RsrConnection methodsFor!
releaseOid: anOid	| command |	self isOpen		ifFalse: [^self].	command := RsrReleaseObjects oids: (Array with: anOid).	command encodeUsing: self encoder.	commandWriter enqueue: command! !

!RsrConnection methodsFor!
_forwarderClass	^RsrForwarder! !

!RsrConnection methodsFor!
transactionSpigot: anObject	transactionSpigot := anObject! !

!RsrConnection methodsFor!
open	(isOpen := socket isConnected)		ifFalse: [^RsrConnectionClosed signal].	stream := RsrSocketStream on: socket.	dispatcher := RsrCommandDispatcher on: self.	commandReader := RsrCommandReader on: self.	commandWriter := RsrCommandWriter on: self.	dispatcher start.	commandReader start.	commandWriter start.	serviceFactory := RsrServiceFactory		_id: self oidSpigot next		connection: self.	serviceFactory _addTo: registry! !

!RsrConnection methodsFor!
isOpen	^isOpen! !

!RsrConnection methodsFor!
socket: aSocket	socket := aSocket! !

!RsrThreadSafeNumericSpigot methodsFor!
initialize	super initialize.	mutex := Semaphore forMutualExclusion! !

!RsrThreadSafeNumericSpigot methodsFor!
next	^mutex critical: [super next]! !

!RsrCommandWriter methodsFor!
write: aByteArray	self stream nextPutAll: aByteArray! !

!RsrCommandWriter methodsFor!
flush	self stream flush! !

!RsrCommandWriter methodsFor!
initialize	super initialize.	queue := SharedQueue new! !

!RsrCommandWriter methodsFor!
enqueue: aCommand	self isActive ifTrue: [queue nextPut: aCommand]! !

!RsrCommandWriter methodsFor!
stopToken	^self stoppedState! !

!RsrCommandWriter methodsFor!
stop	super stop.	queue nextPut: self stopToken! !

!RsrCommandWriter methodsFor!
executeCycle	[| item |	item := queue next.	item == self stopToken		ifTrue: [^self].	item writeUsing: self.	(queue size = 0)		ifTrue: [self flush]]		on: RsrConnectionClosed		do:			[:ex |			self log: ex.			self connection disconnected]! !

!RsrCodec methodsFor!
retainObjectCommand	^0! !

!RsrCodec methodsFor!
deliverResponseCommand	^2! !

!RsrCodec methodsFor!
controlWordMax	^(2 raisedTo: 63) - 1! !

!RsrCodec methodsFor!
sizeOfInteger	"Return the number of bytes used to encode an integer"	^8! !

!RsrCodec methodsFor!
sendMessageCommand	^1! !

!RsrCodec methodsFor!
immediateOID	^0! !

!RsrCodec methodsFor!
releaseObjectsCommand	^3! !

!RsrCodec methodsFor!
controlWordMin	^(2 raisedTo: 63) negated! !

!RsrDecoder methodsFor!
bytesAsInteger: bytes	| res |	res := 0.	bytes do: [:e | res := (res bitShift: 8) bitOr: e].	^res! !

!RsrDecoder methodsFor!
decodeCommand: aStream	"Decode an object from the stream"	| command decodeSelector |	command := self decodeControlWord: aStream.	decodeSelector := self decodeCommandMap		at: command		ifAbsent: [self error: 'Invalid command received'].	^self		perform: decodeSelector		with: aStream! !

!RsrDecoder methodsFor!
signalUnknownOID	RsrUnknownOID signal! !

!RsrDecoder methodsFor!
decodeControlWord: aStream	| bytes unsignedResult |	bytes := aStream next: self sizeOfInteger.	unsignedResult := self bytesAsInteger: bytes.	^unsignedResult > self controlWordMax		ifTrue: [(2 raisedTo: 64) negated + unsignedResult]		ifFalse: [unsignedResult]! !

!RsrDecoder methodsFor!
connection	^connection! !

!RsrDecoder methodsFor!
decodeReleaseObjects: aStream	| count oids |	count := self decodeControlWord: aStream.	oids := Array new: count.	1		to: count		do:			[:i | | oid |			oid := self decodeControlWord: aStream.			oids at: i put: oid].	^RsrReleaseObjects oids: oids! !

!RsrDecoder methodsFor!
decodeSendMessage: aStream	| transaction argCount receiverOID receiver selector arguments |	transaction := self decodeControlWord: aStream.	argCount := self decodeControlWord: aStream.	receiverOID := self decodeControlWord: aStream.	receiver := registry at: receiverOID ifAbsent: [^self signalUnknownOID].	selector := self decodeObjectReference: aStream.	arguments := (1 to: argCount) collect: [:each | self decodeObjectReference: aStream].	^RsrSendMessage		transaction: transaction		receiver: receiver		selector: selector		arguments: arguments! !

!RsrDecoder methodsFor!
registry: anRsrRegistry	registry := anRsrRegistry! !

!RsrDecoder methodsFor!
decodeObjectReference: aStream	| oid |	oid := self decodeControlWord: aStream.	oid = self immediateOID ifTrue: [^self decodeImmediateObject: aStream].	^registry at: oid ifAbsent: [self signalUnknownOID]! !

!RsrDecoder methodsFor!
registry	^registry! !

!RsrDecoder methodsFor!
lookupClass: aClassName	^RsrClassResolver classNamed: aClassName! !

!RsrDecoder methodsFor!
initializeDecodeCommandMap	decodeCommandMap := Dictionary new.	decodeCommandMap		at: self retainObjectCommand put: #decodeRetainObject:;		at: self sendMessageCommand put: #decodeSendMessage:;		at: self deliverResponseCommand put: #decodeDeliverResponse:;		at: self releaseObjectsCommand put: #decodeReleaseObjects:.	^decodeCommandMap! !

!RsrDecoder methodsFor!
decodeImmediateObject: aStream	| species |	species := self decodeControlWord: aStream.	^(RsrSpecies speciesList at: species + 1)		decodeReference: aStream		using: self! !

!RsrDecoder methodsFor!
decodeService: aStream	^RsrServiceSpecies		decode: aStream		using: self! !

!RsrDecoder methodsFor!
decodeRetainObject: aStream	^RsrRetainObject object: (self decodeService: aStream)! !

!RsrDecoder methodsFor!
connection: aConnection	connection := aConnection! !

!RsrDecoder methodsFor!
decodeCommandMap	^decodeCommandMap ifNil: [self initializeDecodeCommandMap]! !

!RsrDecoder methodsFor!
decodeDeliverResponse: aStream	| transaction errorName response |	transaction := self decodeControlWord: aStream.	errorName := self decodeObjectReference: aStream.	response := self decodeObjectReference: aStream.	^RsrDeliverResponse new		transaction: transaction;		errorName: errorName;		response: response;		yourself! !

!RsrSocketStream methodsFor!
isConnected	^socket isConnected! !

!RsrSocketStream methodsFor!
socket: anRsrSocket	socket := anRsrSocket! !

!RsrSocketStream methodsFor!
nextPutAll: bytes	socket write: bytes! !

!RsrSocketStream methodsFor!
close	socket close! !

!RsrSocketStream methodsFor!
next	^self next: 1! !

!RsrSocketStream methodsFor!
flush	"NOP"! !

!RsrSocketStream methodsFor!
next: aCount	aCount = 0		ifTrue: [^#[]].	^[socket read: aCount]		on: RsrConnectionClosed		do: [:ex | ex return: nil]! !

!RsrSocketStream methodsFor!
atEnd	^self isConnected! !

!RsrCommand methodsFor!
encoding	^ encoding! !

!RsrCommand methodsFor!
executeFor: aConnection	self subclassResponsibility! !

!RsrCommand methodsFor!
writeUsing: aCommandWriter	aCommandWriter write: encoding! !

!RsrCommand methodsFor!
sendOver: aConnection	"Do nothing unless a subclass offers a specialization"! !

!RsrCommand methodsFor!
encodeUsing: anRsrEncoder	self subclassResponsibility! !

!RsrCommand methodsFor!
encoding: anObject	encoding := anObject! !

!RsrCommandDispatcher methodsFor!
initialize	super initialize.	queue := SharedQueue new! !

!RsrCommandDispatcher methodsFor!
stopToken	^self stoppedState! !

!RsrCommandDispatcher methodsFor!
priority	^super priority - 1! !

!RsrCommandDispatcher methodsFor!
dispatch: aCommand	queue nextPut: aCommand! !

!RsrCommandDispatcher methodsFor!
stop	super stop.	queue nextPut: self stopToken! !

!RsrCommandDispatcher methodsFor!
executeCycle	| item |	item := queue next.	item == self stopToken		ifFalse: [item executeFor: connection]! !

!RsrCommandProcessor methodsFor!
stop	self isActive ifFalse: [^self].	state := self stoppedState.	self connection close.	self stream close! !

!RsrCommandProcessor methodsFor!
log: anException	self log error: anException description! !

!RsrCommandProcessor methodsFor!
executeCycle	self subclassResponsibility! !

!RsrCommandProcessor methodsFor!
start	state := self runningState.	process := RsrConcurrency		fork: [self runLoop]		at: self priority! !

!RsrCommandProcessor methodsFor!
stoppedState	^#Stop! !

!RsrCommandProcessor methodsFor!
initialize	super initialize.	state := self stoppedState! !

!RsrCommandProcessor methodsFor!
priority	^Processor lowIOPriority! !

!RsrCommandProcessor methodsFor!
connection: anObject	connection := anObject! !

!RsrCommandProcessor methodsFor!
runningState	^#Running! !

!RsrCommandProcessor methodsFor!
runLoop	[self isActive]		whileTrue:			[[self executeCycle]				on: Error				do:					[:ex |					self log: ex.					self connection unknownError: ex]]! !

!RsrCommandProcessor methodsFor!
isActive	^state == self runningState! !

!RsrCommandProcessor methodsFor!
log	^self connection log! !

!RsrCommandProcessor methodsFor!
stream	^self connection stream! !

!RsrCommandProcessor methodsFor!
connection	^connection! !