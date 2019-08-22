run
Object
	subclass: #RsrReflection
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrProtoObject
	subclass: #RsrForwarder
	instVarNames: #(#rsrObject)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrStream
	instVarNames: #(#stream)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrNumericSpigot
	instVarNames: #(#current #step)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrSharedNamespace
	instVarNames: #(#map #client #server)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrLog
	instVarNames: #(#verbosity)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrService
	instVarNames: #(#rsrId #rsrConnection #remoteSelf)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrCommand
	instVarNames: #(#encoding)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrSocketStream
	instVarNames: #(#socket)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrConnection
	instVarNames: #(#isOpen #forwarderClass #transactionSpigot #commandWriter #commandReader #registry #objectCache #socket #stream #promises #dispatcher #oidSpigot #serviceFactory #sharedNamespace #log)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrRetainAnalysis
	instVarNames: #(#roots #retainCommands #inFlight #connection)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrCodec
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrPromise
	instVarNames: #(#mutex #value #error #markerValue)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrCommandProcessor
	instVarNames: #(#process #connection #isRunning)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrObjectCache
	instVarNames: #(#storage)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCommand
	subclass: #RsrRetainObject
	instVarNames: #(#object)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrNumericSpigot
	subclass: #RsrThreadSafeNumericSpigot
	instVarNames: #(#mutex)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCommand
	subclass: #RsrReleaseObjects
	instVarNames: #(#oids)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
Error
	subclass: #RsrError
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCommand
	subclass: #RsrDeliverResponse
	instVarNames: #(#transaction #errorName #response #retainList)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCodec
	subclass: #RsrEncoder
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCommandProcessor
	subclass: #RsrCommandWriter
	instVarNames: #(#queue)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCodec
	subclass: #RsrDecoder
	instVarNames: #(#registry #connection)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrService
	subclass: #RsrAbstractSharedNamespace
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCommandProcessor
	subclass: #RsrCommandReader
	instVarNames: #(#decoder)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCommandProcessor
	subclass: #RsrCommandDispatcher
	instVarNames: #(#queue)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrService
	subclass: #RsrAbstractServiceFactory
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCommand
	subclass: #RsrSendMessage
	instVarNames: #(#transaction #receiver #selector #arguments #retainList)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrAbstractSharedNamespace
	subclass: #RsrSharedNamespaceServer
	instVarNames: #(#namespace)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrAbstractSharedNamespace
	subclass: #RsrSharedNamespaceClient
	instVarNames: #(#mutex #services)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrAbstractServiceFactory
	subclass: #RsrServiceFactoryServer
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrError
	subclass: #RsrUnsupportedObject
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrAbstractServiceFactory
	subclass: #RsrServiceFactory
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrError
	subclass: #RsrUnknownOID
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrError
	subclass: #RsrCycleDetected
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%





set class RsrReflection class

classmethod:
reflectedVariableNamesOf: aService	^aService class variablesToReflect
%



set class RsrReflection class

classmethod:
reflectedVariableIndecesOf: anRsrObjectdo: aBlock	| allVariables |	allVariables := anRsrObject class allInstVarNames.	anRsrObject class variablesToReflect		do:			[:varName | | index |			index := allVariables indexOf: varName.			aBlock value: index]
%



set class RsrReflection class

classmethod:
reflectedVariablesOf: anRsrObjectdo: aBlock	RsrReflection		reflectedVariableIndecesOf: anRsrObject		do:			[:index |			aBlock value: (anRsrObject instVarAt: index)]
%



set class RsrReflection class

classmethod:
allInstancesOf: aClass	^(aClass respondsTo: #instancesInMemory)		ifTrue: [aClass instancesInMemory "GemStone"]		ifFalse: [aClass allInstances]
%

set class RsrForwarder class

classmethod:
on: anRsrObject	| instance |	instance := self new.	instance rsrObject: anRsrObject.	^instance
%

set class RsrForwarder

method:
doesNotUnderstand: aMessage	| promise |	promise := rsrObject rsrConnection		rsrSendMessage: aMessage		to: rsrObject.	^promise value
%



set class RsrForwarder

method:
rsrObject: anRsrObject	rsrObject := anRsrObject
%

set class RsrStream class

classmethod:
on: aStream	^self new		stream: aStream;		yourself
%

set class RsrStream

method:
nextPutAll: aByteArray	^stream nextPutAll: aByteArray
%



set class RsrStream

method:
stream: aStream	stream := aStream
%



set class RsrStream

method:
next	^self next: 1
%



set class RsrStream

method:
next: aLength	| bytes |	bytes := stream next: aLength.	bytes size ~~ aLength		ifTrue: [RsrConnectionClosed signal].	^bytes
%



set class RsrStream

method:
flush	stream flush
%



set class RsrStream

method:
binary	stream binary
%



set class RsrStream

method:
close	stream close
%

set class RsrNumericSpigot class

classmethod:
naturals	^self		start: 1		step: 1
%



set class RsrNumericSpigot class

classmethod:
new	^self		start: 0		step: 1
%



set class RsrNumericSpigot class

classmethod:
start: aNumberstep: anIncrement	^super new		first: aNumber;		step: anIncrement;		yourself
%

set class RsrNumericSpigot

method:
next	| result |	result := current.	current := current + step.	^result
%



set class RsrNumericSpigot

method:
step: anIncrement	step := anIncrement
%



set class RsrNumericSpigot

method:
first: aNumber	current := aNumber
%



set class RsrNumericSpigot

method:
negated	^self class		start: current negated		step: step negated
%



set class RsrNumericSpigot

method:
next: aCount	| result |	result := Array new: aCount.	1 to: aCount do: [:i | result at: i put: self next].	^result
%



set class RsrNumericSpigot

method:
step	^step
%

set class RsrSharedNamespace

method:
at: aKey	^map at: aKey
%



set class RsrSharedNamespace

method:
client	^ client
%



set class RsrSharedNamespace

method:
primAt: aKeyput: aValue	^map		at: aKey		put: aValue
%



set class RsrSharedNamespace

method:
initialize	super initialize.	map := Dictionary new
%



set class RsrSharedNamespace

method:
removeKey: aKey	^[self primRemoveKey: aKey]		ensure: [client removeKey: aKey]
%



set class RsrSharedNamespace

method:
server	^ server
%



set class RsrSharedNamespace

method:
at: aKeyput: aValue	self		primAt: aKey		put: aValue.	client		at: aKey		put: aValue
%



set class RsrSharedNamespace

method:
at: aKeyifAbsent: aBlock	^map at: aKey ifAbsent: aBlock
%



set class RsrSharedNamespace

method:
server: anObject	server := anObject
%



set class RsrSharedNamespace

method:
primRemoveKey: aKey	^map removeKey: aKey
%



set class RsrSharedNamespace

method:
map	^ map
%



set class RsrSharedNamespace

method:
client: anObject	client := anObject
%



set class RsrSharedNamespace

method:
map: anObject	map := anObject
%

set class RsrLog

method:
levelInfo	^3
%



set class RsrLog

method:
verbosity	^verbosity
%



set class RsrLog

method:
info: aString	self verbosity >= self levelInfo		ifTrue: [self log: 'INFO: ', aString]
%



set class RsrLog

method:
error: aString	self verbosity >= self levelError		ifTrue: [self log: 'ERROR: ', aString]
%



set class RsrLog

method:
initialize	super initialize.	verbosity := self levelTrace
%



set class RsrLog

method:
log: aString	Transcript		show: RsrDateTimeInterface now printString, '-', aString;		cr
%



set class RsrLog

method:
warn: aString	self verbosity >= self levelDebug		ifTrue: [self log: 'WARN: ', aString]
%



set class RsrLog

method:
verbosity: aLogLevel	verbosity := aLogLevel
%



set class RsrLog

method:
log: aMessagelevel: aLevelString	Transcript		show: RsrDateTimeInterface now printString, '-', aLevelString, '-', aMessage;		cr
%



set class RsrLog

method:
levelDebug	^4
%



set class RsrLog

method:
trace: aString	self verbosity >= self levelTrace		ifTrue: [self log: 'TRACE: ', aString]
%



set class RsrLog

method:
levelTrace	^5
%



set class RsrLog

method:
debug: aString	self verbosity >= self levelDebug		ifTrue: [	self log: 'DEBUG: ', aString]
%



set class RsrLog

method:
levelError	^1
%



set class RsrLog

method:
levelWarn	^2
%

set class RsrService class

classmethod:
isClientClass	^self == self clientClass
%



set class RsrService class

classmethod:
serverClass	self subclassResponsibility
%



set class RsrService class

classmethod:
rsrId: anIdrsrConnection: aConnection	^super new		rsrId: anId;		rsrConnection: aConnection;		yourself
%



set class RsrService class

classmethod:
clientClass	self subclassResponsibility
%



set class RsrService class

classmethod:
variablesToReflect	| currentClass variables |	variables := OrderedCollection new.	currentClass := self superclass.	[currentClass == RsrService]		whileFalse:			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].			currentClass := currentClass superclass].	^variables
%



set class RsrService class

classmethod:
isServerClass	^self == self serverClass
%

set class RsrService

method:
rsrConnection: aConnection	rsrConnection := aConnection.	remoteSelf := aConnection rsrForwarderClass on: self
%



set class RsrService

method:
scientist	^RsrScientist new
%



set class RsrService

method:
rsrConnection	^rsrConnection
%



set class RsrService

method:
serviceName	^self class name
%



set class RsrService

method:
remoteServiceName	^self remoteServiceClass name
%



set class RsrService

method:
registerWith: aRegistry	self isServer		ifTrue: [aRegistry retain: self]		ifFalse: [aRegistry register: self]
%



set class RsrService

method:
isClient	^self class isClientClass
%



set class RsrService

method:
synchronize	remoteSelf yourself
%



set class RsrService

method:
remoteServiceClass	^self isClient		ifTrue: [self class serverClass]		ifFalse: [self class clientClass]
%



set class RsrService

method:
rsrId: anRsrId	rsrId := anRsrId
%



set class RsrService

method:
isMirrored	^rsrConnection ~~ nil
%



set class RsrService

method:
isNotMirrored	^self isMirrored not
%



set class RsrService

method:
isServer	^self class isServerClass
%



set class RsrService

method:
rsrId	^rsrId
%

set class RsrCommand

method:
scientist	^RsrScientist new
%



set class RsrCommand

method:
writeUsing: aCommandWriter	aCommandWriter write: encoding
%



set class RsrCommand

method:
encoding	^ encoding
%



set class RsrCommand

method:
encodeUsing: anRsrEncoder	self subclassResponsibility
%



set class RsrCommand

method:
executeFor: aConnection	self subclassResponsibility
%



set class RsrCommand

method:
encoding: anObject	encoding := anObject
%

set class RsrSocketStream class

classmethod:
on: anRsrSocket	^self new		socket: anRsrSocket;		yourself
%

set class RsrSocketStream

method:
nextPutAll: bytes	socket write: bytes
%



set class RsrSocketStream

method:
next	^self next: 1
%



set class RsrSocketStream

method:
isConnected	^socket isConnected
%



set class RsrSocketStream

method:
socket: anRsrSocket	socket := anRsrSocket
%



set class RsrSocketStream

method:
next: aCount	^[socket read: aCount]		on: RsrConnectionClosed		do: [:ex | ex return: nil]
%



set class RsrSocketStream

method:
flush	"NOP"
%



set class RsrSocketStream

method:
close	socket close
%



set class RsrSocketStream

method:
atEnd	^self isConnected
%

set class RsrConnection class

classmethod:
connectTo: aPortNumberon: aHostname	| socket |	socket := RsrSocket new.	socket		connectTo: aPortNumber		on: aHostname.	^(self		socket: socket		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1) negated) start
%



set class RsrConnection class

classmethod:
acceptOn: aPortNumber	| listener socket |	listener := RsrSocket new.	listener listenOn: aPortNumber.	socket := listener accept.	^(self		socket: socket		transactionSpigot: RsrThreadSafeNumericSpigot naturals		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1)) start
%



set class RsrConnection class

classmethod:
new	self error
%



set class RsrConnection class

classmethod:
socket: aSockettransactionSpigot: aNumericSpigotoidSpigot: anOidSpigot	^super new		socket: aSocket;		transactionSpigot: aNumericSpigot;		oidSpigot: anOidSpigot;		yourself
%



set class RsrConnection class

classmethod:
connectionTimeout	^2
%

set class RsrConnection

method:
close	isOpen		ifFalse: [^self].	isOpen := false.	commandReader stop.	commandWriter stop.	dispatcher stop.	self promises do: [:each | each error: RsrConnectionClosed new].	objectCache reset
%



set class RsrConnection

method:
forwarderClass	^forwarderClass ifNil: [RsrForwarder]
%



set class RsrConnection

method:
commandReader: aCommandReader	commandReader := aCommandReader
%



set class RsrConnection

method:
log	^log
%



set class RsrConnection

method:
newTransactionId	^transactionSpigot next
%



set class RsrConnection

method:
encoder	^RsrEncoder new
%



set class RsrConnection

method:
registry	^registry
%



set class RsrConnection

method:
rsrForwarderClass: aClass	^self forwarderClass: aClass
%



set class RsrConnection

method:
objectCache	^objectCache
%



set class RsrConnection

method:
sharedNamespace	^sharedNamespace
%



set class RsrConnection

method:
transactionSpigot	^transactionSpigot
%



set class RsrConnection

method:
unknownError: anException	self close
%



set class RsrConnection

method:
dispatcher	^dispatcher
%



set class RsrConnection

method:
rsrForwarderClass	^self forwarderClass
%



set class RsrConnection

method:
commandReader	^commandReader
%



set class RsrConnection

method:
transactionSpigot: anObject	transactionSpigot := anObject
%



set class RsrConnection

method:
oidSpigot	^oidSpigot
%



set class RsrConnection

method:
decoder	^RsrDecoder registry: registry connection: self
%



set class RsrConnection

method:
isOpen	^isOpen
%



set class RsrConnection

method:
commandWriter: anRsrCommandWriter	commandWriter := anRsrCommandWriter
%



set class RsrConnection

method:
initialize	super initialize.	isOpen := false.	transactionSpigot := RsrThreadSafeNumericSpigot naturals.	objectCache := RsrObjectCache new.	promises := Dictionary new.	registry := RsrRegistry reapAction: [:oid | self releaseOid: oid].	sharedNamespace := RsrSharedNamespace new.	log := RsrLog new
%



set class RsrConnection

method:
disconnected	self log info: 'Disconnected'.	self close
%



set class RsrConnection

method:
releaseOid: anOid	| command |	self isOpen		ifFalse: [^self].	command := RsrReleaseObjects oids: (Array with: anOid).	command encodeUsing: self encoder.	commandWriter enqueue: command
%



set class RsrConnection

method:
oidSpigot: anIntegerSpigot	oidSpigot := anIntegerSpigot
%



set class RsrConnection

method:
rsrSendMessage: aMessageto: aService"Open coordination window"	"Send dirty transitive closure of aRemoteMessage"	"Send DispatchMessage command""Coorination window closed"	"Return Promise"	| dispatchCommand |	isOpen		ifFalse: [self error: 'Connection is not open'].	dispatchCommand := RsrSendMessage		transaction: self newTransactionId		receiver: aService		selector: aMessage selector		arguments: aMessage arguments.	^dispatchCommand sendOver: self
%



set class RsrConnection

method:
socket: aSocket	socket := aSocket
%



set class RsrConnection

method:
promises	^promises
%



set class RsrConnection

method:
forwarderClass: anObject	forwarderClass := anObject
%



set class RsrConnection

method:
commandWriter	^commandWriter
%



set class RsrConnection

method:
serviceFor: aResponsibility	^serviceFactory serviceFor: aResponsibility
%



set class RsrConnection

method:
stream	^stream
%



set class RsrConnection

method:
start	(isOpen := socket isConnected)		ifFalse: [^RsrConnectionClosed signal].	stream := RsrSocketStream on: socket.	dispatcher := RsrCommandDispatcher on: self.	commandReader := RsrCommandReader on: self.	commandWriter := RsrCommandWriter on: self.	dispatcher start.	commandReader start.	commandWriter start.	serviceFactory := RsrServiceFactory		rsrId: self oidSpigot next		rsrConnection: self.	serviceFactory registerWith: registry.	sharedNamespace client: (self serviceFor: #RsrSharedNamespaceClient)
%

set class RsrRetainAnalysis class

classmethod:
roots: anArrayconnection: aConnection	^self new		roots: anArray;		connection: aConnection;		yourself
%

set class RsrRetainAnalysis

method:
processDataObject: aDataObject	^self
%



set class RsrRetainAnalysis

method:
process: anObject	(self isDataObject: anObject)		ifTrue: [^self processDataObject: anObject].	(self isService: anObject)		ifTrue: [^self processRsrObject: anObject].	(self isCollection: anObject)		ifTrue: [^self processCollection: anObject].	(self isImmediate: anObject)		ifTrue: [^anObject].	^RsrUnsupportedObject signal: 'Unsupported object (' , anObject printString , ')'
%



set class RsrRetainAnalysis

method:
processDictionary: aDictionary	aDictionary		keysAndValuesDo:			[:key :value |			self				process: key;				process: value].	^aDictionary
%



set class RsrRetainAnalysis

method:
encoder	^self connection encoder
%



set class RsrRetainAnalysis

method:
perform	roots do: [:each | self process: each]
%



set class RsrRetainAnalysis

method:
registry	^connection registry
%



set class RsrRetainAnalysis

method:
ensureRegistered: anRsrObject	anRsrObject isMirrored		ifFalse:			[anRsrObject				rsrId: self nextOid;				rsrConnection: self connection.			anRsrObject registerWith: self registry]
%



set class RsrRetainAnalysis

method:
isCollection: anObject	^anObject isCollection
%



set class RsrRetainAnalysis

method:
processing: anObjectduring: aBlock	(inFlight includes: anObject)		ifTrue: [^RsrCycleDetected signal: 'Cycled detected on: ', anObject printString].	inFlight add: anObject.	self ensureRegistered: anObject.	aBlock value.	self retain: anObject.	inFlight remove: anObject
%



set class RsrRetainAnalysis

method:
isDataObject: anObject	^anObject == true		or: [anObject == false			or: [anObject == nil				or: [(self isString: anObject)					or: [(self isSymbol: anObject)						or: [(self isInteger: anObject)]]]]]
%



set class RsrRetainAnalysis

method:
initialize	super initialize.	retainCommands := OrderedCollection new.	inFlight := IdentitySet new
%



set class RsrRetainAnalysis

method:
processCollection: aCollection	(self isDictionary: aCollection)		ifTrue: [^self processDictionary: aCollection].	aCollection do: [:each | self process: each].	^aCollection
%



set class RsrRetainAnalysis

method:
retainCommands	^retainCommands
%



set class RsrRetainAnalysis

method:
roots	^roots
%



set class RsrRetainAnalysis

method:
isImmediate: anObject	^self encoder isImmediate: anObject
%



set class RsrRetainAnalysis

method:
connection	^connection
%



set class RsrRetainAnalysis

method:
processRsrObject: anRsrObject	self		processing: anRsrObject		during:			[RsrReflection				reflectedVariablesOf: anRsrObject				do: [:each | self process: each]]
%



set class RsrRetainAnalysis

method:
retain: anRsrObject	| retainCommand |	retainCommand := RsrRetainObject object: anRsrObject.	retainCommand encodeUsing: self encoder.	self retainCommands add: retainCommand
%



set class RsrRetainAnalysis

method:
isService: anObject	^anObject isKindOf: RsrService
%



set class RsrRetainAnalysis

method:
connection: aConnection	connection := aConnection
%



set class RsrRetainAnalysis

method:
roots: anObject	roots := anObject
%



set class RsrRetainAnalysis

method:
nextOid	^self connection oidSpigot next
%

set class RsrCodec

method:
dictionaryType	^13
%



set class RsrCodec

method:
byteArrayType	^10
%



set class RsrCodec

method:
releaseObjectsCommand	^3
%



set class RsrCodec

method:
dateTimeType	^14
%



set class RsrCodec

method:
positiveIntegerType	^3
%



set class RsrCodec

method:
controlWordMin	^(2 raisedTo: 63) negated
%



set class RsrCodec

method:
serviceType	^0
%



set class RsrCodec

method:
characterType	^5
%



set class RsrCodec

method:
retainObjectCommand	^0
%



set class RsrCodec

method:
setType	^11
%



set class RsrCodec

method:
trueType	^7
%



set class RsrCodec

method:
negativeIntegerType	^4
%



set class RsrCodec

method:
falseType	^8
%



set class RsrCodec

method:
symbolType	^1
%



set class RsrCodec

method:
deliverResponseCommand	^2
%



set class RsrCodec

method:
orderedCollectionType	^12
%



set class RsrCodec

method:
nilType	^6
%



set class RsrCodec

method:
sizeOfInteger	"Return the number of bytes used to encode an integer"	^8
%



set class RsrCodec

method:
isImmediate: anObject	^(self isSymbol: anObject)		or: [(self isString: anObject)			or: [(self isInteger: anObject)				or: [(self isCharacter: anObject)					or: [({Array. Dictionary. ByteArray. Set. OrderedCollection. RsrDateTimeInterface dateTimeClass.} includes: anObject class)						or: [#(nil true false) includes: anObject]]]]]
%



set class RsrCodec

method:
arrayType	^9
%



set class RsrCodec

method:
controlWordMax	^(2 raisedTo: 63) -1
%



set class RsrCodec

method:
sendMessageCommand	^1
%



set class RsrCodec

method:
stringType	^2
%

set class RsrPromise

method:
error: anException	self isFulfilled		ifTrue: [^self error: 'Promise value already set'].	error := anException.	mutex signal	
%



set class RsrPromise

method:
initialize	super initialize.	value := markerValue := Object new.	mutex := Semaphore new
%



set class RsrPromise

method:
value	self waitForFulfillment.	error isNil		ifFalse: [error copy signal].	^value
%



set class RsrPromise

method:
fulfill: anObject	self isFulfilled		ifTrue: [^self error: 'Promise value already set'].	value := anObject.	mutex signal
%



set class RsrPromise

method:
isFulfilled	^value ~~ markerValue
%



set class RsrPromise

method:
waitForFulfillment	self isFulfilled		ifTrue: [^self].	mutex wait.	mutex signal
%

set class RsrCommandProcessor class

classmethod:
on: aConnection	^self new		connection: aConnection;		yourself
%

set class RsrCommandProcessor

method:
priority	^Processor lowIOPriority
%



set class RsrCommandProcessor

method:
log: anException	self log error: anException description
%



set class RsrCommandProcessor

method:
initialize	super initialize.	isRunning := false
%



set class RsrCommandProcessor

method:
log	^self connection log
%



set class RsrCommandProcessor

method:
executeCycle	self subclassResponsibility
%



set class RsrCommandProcessor

method:
connection	^connection
%



set class RsrCommandProcessor

method:
stop	isRunning ifFalse: [^self].	isRunning := false.	self connection close.	self stream close
%



set class RsrCommandProcessor

method:
connection: anObject	connection := anObject
%



set class RsrCommandProcessor

method:
runLoop	[isRunning]		whileTrue:			[[self executeCycle]				on: Error				do:					[:ex |					self log: ex.					self connection unknownError: ex]]
%



set class RsrCommandProcessor

method:
stream	^self connection stream
%



set class RsrCommandProcessor

method:
start	isRunning := true.	process := RsrConcurrency		fork: [self runLoop]		at: self priority
%

set class RsrObjectCache

method:
add: anObject	storage add: anObject
%



set class RsrObjectCache

method:
initialize	super initialize.	self reset
%



set class RsrObjectCache

method:
reset	storage := IdentitySet new
%

set class RsrRetainObject class

classmethod:
object: anRsrObjectencoding: aByteArray	^self new		object: anRsrObject;		encoding: aByteArray;		yourself
%



set class RsrRetainObject class

classmethod:
object: anRsrObject	^self new		object: anRsrObject;		yourself
%

set class RsrRetainObject

method:
object	^ object
%



set class RsrRetainObject

method:
hash	^self object hash
%



set class RsrRetainObject

method:
writeUsing: aCommandWriter	super writeUsing: aCommandWriter
%



set class RsrRetainObject

method:
= anEncodedObject	self == anEncodedObject		ifTrue: [^true].	self class == anEncodedObject class		ifFalse: [^false].	^self object = anEncodedObject object		and: [self encoding = anEncodedObject encoding]
%



set class RsrRetainObject

method:
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeRetainObject: self
%



set class RsrRetainObject

method:
executeFor: aConnection	aConnection objectCache add: object
%



set class RsrRetainObject

method:
object: anObject	object := anObject
%

set class RsrThreadSafeNumericSpigot

method:
initialize	super initialize.	mutex := Semaphore forMutualExclusion
%



set class RsrThreadSafeNumericSpigot

method:
next	^mutex critical: [super next]
%

set class RsrReleaseObjects class

classmethod:
oids: anArray	^self new		oids: anArray;		yourself
%

set class RsrReleaseObjects

method:
oids	^oids
%



set class RsrReleaseObjects

method:
oids: anArray	oids := anArray
%



set class RsrReleaseObjects

method:
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeReleaseObjects: self
%



set class RsrReleaseObjects

method:
executeFor: aConnection	| registry |	registry := aConnection registry.	oids do: [:oid | registry removeKey: oid]
%

set class RsrDeliverResponse class

classmethod:
transaction: aTransactionIdresponse: anObject	^self new		transaction: aTransactionId;		response: anObject;		yourself
%



set class RsrDeliverResponse class

classmethod:
transaction: aTransactionIderror: anException	^self new		transaction: aTransactionId;		errorName: anException class name;		response: anException messageText;		yourself
%

set class RsrDeliverResponse

method:
sendOver: aConnection	| analysis |	analysis := RsrRetainAnalysis		roots: (Array with: response)		connection: aConnection.	analysis perform.	retainList := analysis retainCommands.	self encodeUsing: aConnection encoder.	aConnection commandWriter enqueue: self
%



set class RsrDeliverResponse

method:
error	^(RsrClassResolver classNamed: errorName ifAbsent: [RsrError]) new		messageText: response;		yourself
%



set class RsrDeliverResponse

method:
isError	^errorName notNil
%



set class RsrDeliverResponse

method:
response	^response
%



set class RsrDeliverResponse

method:
transaction	^transaction
%



set class RsrDeliverResponse

method:
writeUsing: aCommandWriter	retainList do: [:each | each writeUsing: aCommandWriter].	aCommandWriter write: encoding
%



set class RsrDeliverResponse

method:
errorName	^errorName
%



set class RsrDeliverResponse

method:
encodeUsing: anRsrEncoder	encoding := anRsrEncoder encodeDeliverResponse: self
%



set class RsrDeliverResponse

method:
response: anObject	response := anObject
%



set class RsrDeliverResponse

method:
transaction: aTransactionId	transaction := aTransactionId
%



set class RsrDeliverResponse

method:
executeFor: aConnection	| promise |	promise := aConnection promises		removeKey: transaction		ifAbsent:			[^self error: 'Handle unknown transaction'].	self isError		ifTrue: [promise error: self error]		ifFalse: [promise fulfill: response].	aConnection objectCache reset	
%



set class RsrDeliverResponse

method:
errorName: aSymbol	errorName := aSymbol
%

set class RsrEncoder

method:
encodeCharacter: aCharacteronto: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self characterType		onto: aStream.	self		encodeControlWord: aCharacter codePoint		onto: aStream
%



set class RsrEncoder

method:
encodeRetainObject: aRetainObject	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self retainObjectIdentifier				onto: stream.			self				encodeObject: aRetainObject object				onto: stream]
%



set class RsrEncoder

method:
encodeReleaseObjects: aReleaseObject	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self releaseObjectsCommand				onto: stream.			self				encodeControlWord: aReleaseObject oids size				onto: stream.			aReleaseObject oids				do:					[:oid |					self						encodeControlWord: oid						onto: stream]]
%



set class RsrEncoder

method:
encodeSet: aSeton: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self setType		onto: aStream.	self		encodeControlWord: aSet size		onto: aStream.	aSet		do:			[:each |			self				encodeReferenceOf: each				onto: aStream]
%



set class RsrEncoder

method:
sendMessageIdentifier	^1
%



set class RsrEncoder

method:
encodeService: aServiceon: aStream	"type"	"the OID for the object"	"the name of the remote service to create"	"Write the object slots"	| reflectedVariables |	reflectedVariables := RsrReflection reflectedVariableNamesOf: aService.	self		encodeControlWord: self serviceType		onto: aStream.	self		encodeControlWord: aService rsrId		onto: aStream.	self		encodeControlWord: reflectedVariables size		onto: aStream.	self		encodeSymbol: aService remoteServiceName		onto: aStream.	RsrReflection		reflectedVariablesOf: aService		do: [:each | self encodeReferenceOf: each onto: aStream]
%



set class RsrEncoder

method:
encodeDeliverResponse: aDeliverResponse	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self deliverResponseCommand				onto: stream.			self				encodeControlWord: aDeliverResponse transaction				onto: stream.			self				encodeReferenceOf: aDeliverResponse errorName				onto: stream.			self				encodeReferenceOf: aDeliverResponse response				onto: stream]
%



set class RsrEncoder

method:
encodeImmediateInteger: anIntegeronto: aStream	| bytes immediateType |	immediateType := anInteger positive		ifTrue: [self positiveIntegerType]		ifFalse: [self negativeIntegerType].	bytes := self integerAsByteArray: anInteger abs.	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: immediateType		onto: aStream.	self		encodeControlWord: bytes size		onto: aStream.	aStream nextPutAll: bytes
%



set class RsrEncoder

method:
encodeObject: anObjectonto: aStream	(self isService: anObject)		ifTrue: [^self encodeService: anObject on: aStream].	self error: 'Unable to encode: ', anObject printString
%



set class RsrEncoder

method:
encodeControlWord: anIntegeronto: aStream	| encodedInteger encodedBytes |	(anInteger between: self controlWordMin and: self controlWordMax)		ifFalse: [self error: anInteger printString, ' is outside the supported size of a control word.'].	encodedInteger := (anInteger positive		ifTrue: [anInteger]		ifFalse: [(2 raisedTo: 64) + anInteger]).	encodedBytes := self		integerAsByteArray: encodedInteger		ofSize: self sizeOfInteger.	aStream nextPutAll: encodedBytes
%



set class RsrEncoder

method:
encodeString: aStringonto: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self stringType		onto: aStream.	self		encodeStringBody: aString		onto: aStream
%



set class RsrEncoder

method:
encodeTrueOnto: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self trueType		onto: aStream
%



set class RsrEncoder

method:
encodeByteArray: aByteArrayon: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self byteArrayType		onto: aStream.	self		encodeControlWord: aByteArray size		onto: aStream.	aStream nextPutAll: aByteArray
%



set class RsrEncoder

method:
encodeReferenceOf: anObjectonto: aStream	(self isService: anObject)		ifTrue: [^self encodeControlWord: anObject rsrId onto: aStream].	(self isImmediate: anObject)		ifTrue: [^self encodeImmediate: anObject onto: aStream].	self error: 'Unsupported type'
%



set class RsrEncoder

method:
encodeSendMessage: aSendMessage	^ByteArray		streamContents:			[:stream |			self				encodeControlWord: self sendMessageIdentifier				onto: stream.			self				encodeControlWord: aSendMessage transaction				onto: stream.			self				encodeControlWord: aSendMessage arguments size				onto: stream.			self				encodeReferenceOf: aSendMessage receiver				onto: stream.			self				encodeReferenceOf: aSendMessage selector				onto: stream.			aSendMessage arguments				do:					[:each |					self						encodeReferenceOf: each						onto: stream]]
%



set class RsrEncoder

method:
integerAsByteArray: anIntegerofSize: aNumberOfBytes	| bytes int |	bytes := ByteArray new: aNumberOfBytes.	int := anInteger.	aNumberOfBytes		to: 1		by: -1		do:			[:i | | byte |			byte := int bitAnd: 16rFF.			int := int bitShift: -8.			bytes at: i put: byte].	int ~= 0		ifTrue: [self error: 'Loss of precision detected'].	^bytes
%



set class RsrEncoder

method:
encodeNilOnto: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self nilType		onto: aStream
%



set class RsrEncoder

method:
encodeArray: anArrayon: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self arrayType		onto: aStream.	self		encodeControlWord: anArray size		onto: aStream.	anArray		do:			[:each |			self				encodeReferenceOf: each				onto: aStream]
%



set class RsrEncoder

method:
encodeFalseOnto: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self falseType		onto: aStream
%



set class RsrEncoder

method:
encodeDateTime: aDateTimeon: aStream	| microseconds |	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self dateTimeType		onto: aStream.	microseconds := RsrDateTimeInterface microsecondsSince: aDateTime.	self		encodeControlWord: microseconds		onto: aStream
%



set class RsrEncoder

method:
encodeObject: anObject	^ByteArray		streamContents:			[:stream |			self				encodeObject: anObject				onto: stream]
%



set class RsrEncoder

method:
encodeSymbol: aSymbolonto: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self symbolType		onto: aStream.	self		encodeStringBody: aSymbol		onto: aStream
%



set class RsrEncoder

method:
retainObjectIdentifier	^0
%



set class RsrEncoder

method:
encodeDictionary: aDictionaryon: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self dictionaryType		onto: aStream.	self		encodeControlWord: aDictionary size		onto: aStream.	aDictionary		keysAndValuesDo:			[:key :value |			self				encodeReferenceOf: key				onto: aStream.			self				encodeReferenceOf: value				onto: aStream]
%



set class RsrEncoder

method:
encodeStringBody: aStringonto: aStream	| bytes |	bytes := (aString respondsTo: #utf8Encoded)		ifTrue: [aString utf8Encoded "pharo"]		ifFalse: [aString encodeAsUTF8 "GemStone"].	self		encodeControlWord: bytes size		onto: aStream.	aStream nextPutAll: bytes
%



set class RsrEncoder

method:
isService: anObject	^anObject isKindOf: RsrService
%



set class RsrEncoder

method:
integerAsByteArray: anInteger	"Return a ByteArray representing <anInteger> in big endian format."	| stream int |	anInteger <= 0		ifTrue: [^ByteArray with: 0].	stream := WriteStream on: (ByteArray new: 8).	int := anInteger.	[int > 0]		whileTrue:			[stream nextPut: (int bitAnd: 16rFF).			int := int bitShift: -8].	^stream contents reverse
%



set class RsrEncoder

method:
immediateOID	^0
%



set class RsrEncoder

method:
encodeOrderedCollection: anOrderedCollectionon: aStream	self		encodeControlWord: self immediateOID		onto: aStream.	self		encodeControlWord: self orderedCollectionType		onto: aStream.	self		encodeControlWord: anOrderedCollection size		onto: aStream.	anOrderedCollection		do:			[:each |			self				encodeReferenceOf: each				onto: aStream]
%



set class RsrEncoder

method:
isArray: anObject	^anObject class == Array
%



set class RsrEncoder

method:
encodeImmediate: anImmediateonto: aStream	(self isSymbol: anImmediate)		ifTrue:			[^self				encodeSymbol: anImmediate				onto: aStream].	(self isString: anImmediate)		ifTrue:			[^self				encodeString: anImmediate				onto: aStream].	(self isInteger: anImmediate)		ifTrue:			[^self				encodeImmediateInteger: anImmediate				onto: aStream].	(self isCharacter: anImmediate)		ifTrue:			[^self				encodeCharacter: anImmediate				onto: aStream].	anImmediate == nil		ifTrue:			[^self encodeNilOnto: aStream].	anImmediate == true		ifTrue:			[^self encodeTrueOnto: aStream].	anImmediate == false		ifTrue:			[^self encodeFalseOnto: aStream].	(self isArray: anImmediate)		ifTrue: [^self encodeArray: anImmediate on: aStream].	(self isDictionary: anImmediate)		ifTrue:			[^self				encodeDictionary: anImmediate				on: aStream].	(self isByteArray: anImmediate)		ifTrue:			[^self				encodeByteArray: anImmediate				on: aStream].	(self isSet: anImmediate)		ifTrue:			[^self				encodeSet: anImmediate				on: aStream].	(self isOrderedCollection: anImmediate)		ifTrue:			[^self				encodeOrderedCollection: anImmediate				on: aStream].	(self isDateTime: anImmediate)		ifTrue:			[^self				encodeDateTime: anImmediate				on: aStream].	self error: 'Unsupported Immediate'
%

set class RsrCommandWriter

method:
initialize	super initialize.	queue := SharedQueue new
%



set class RsrCommandWriter

method:
enqueue: aCommand	isRunning ifTrue: [queue nextPut: aCommand]
%



set class RsrCommandWriter

method:
executeCycle	[queue next writeUsing: self.	queue isEmpty		ifTrue: [self flush]]		on: RsrConnectionClosed		do:			[:ex |			self log: ex.			self connection disconnected]
%



set class RsrCommandWriter

method:
write: aByteArray	self stream nextPutAll: aByteArray
%



set class RsrCommandWriter

method:
flush	self stream flush
%

set class RsrDecoder class

classmethod:
registry: anRsrRegistryconnection: aConnection	^self new		registry: anRsrRegistry;		connection: aConnection;		yourself
%

set class RsrDecoder

method:
lookupClass: aClassName	^RsrClassResolver classNamed: aClassName
%



set class RsrDecoder

method:
decodeReleaseObjects: aStream	| count oids |	count := self decodeControlWord: aStream.	oids := Array new: count.	1		to: count		do:			[:i | | oid |			oid := self decodeControlWord: aStream.			oids at: i put: oid].	^RsrReleaseObjects oids: oids
%



set class RsrDecoder

method:
registry	^registry
%



set class RsrDecoder

method:
decodeArray: aStream	| size array |	size := self decodeControlWord: aStream.	array := Array new: size.	1 to: size do: [:i | array at: i put: (self decodeObjectReference: aStream)].	^array
%



set class RsrDecoder

method:
bytesAsInteger: bytes	^bytes		inject: 0		into: [:int :byte | (int bitShift: 8) bitOr: byte]
%



set class RsrDecoder

method:
decodeControlWord: aStream	| bytes unsignedResult |	bytes := aStream next: self sizeOfInteger.	unsignedResult := self bytesAsInteger: bytes.	^unsignedResult > self controlWordMax		ifTrue: [(2 raisedTo: 64) negated + unsignedResult]		ifFalse: [unsignedResult]
%



set class RsrDecoder

method:
decodeService: aStream	| oid instVarCount serviceName instance |	oid := self decodeControlWord: aStream.	instVarCount := self decodeControlWord: aStream.	serviceName := self decodeObjectReference: aStream.	instance := registry		at: oid		ifAbsent:			[((self lookupClass: serviceName)				rsrId: oid				rsrConnection: self connection)					yourself].	instance registerWith: self registry.	(RsrReflection reflectedVariableNamesOf: instance) size = instVarCount		ifFalse: [self error: 'Incorrectly encoded instance detected'].	RsrReflection		reflectedVariableIndecesOf: instance		do: [:index | instance instVarAt: index put: (self decodeObjectReference: aStream)].	^instance
%



set class RsrDecoder

method:
decodeInteger: aStream	| length bytes |	length := self decodeControlWord: aStream.	bytes := aStream next: length.	^bytes asInteger
%



set class RsrDecoder

method:
decodeString: aStream	| length bytes |	length := self decodeControlWord: aStream.	bytes := aStream next: length.	^(bytes respondsTo: #utf8Decoded)		ifTrue: [bytes utf8Decoded "Pharo"]		ifFalse: [bytes decodeFromUTF8ToString "GemStone"]
%



set class RsrDecoder

method:
decodeObjectReference: aStream	| oid |	oid := self decodeControlWord: aStream.	oid = 0 ifTrue: [^self decodeImmediateObject: aStream].	^registry at: oid ifAbsent: [self signalUnknownOID]
%



set class RsrDecoder

method:
decodeSet: aStream	| size set |	size := self decodeControlWord: aStream.	set := Set new: size.	size timesRepeat: [set add: (self decodeObjectReference: aStream)].	^set
%



set class RsrDecoder

method:
decodeCharacter: aStream	| codePoint |	codePoint := self decodeControlWord: aStream.	^Character codePoint: codePoint
%



set class RsrDecoder

method:
decodeDeliverResponse: aStream	| transaction errorName response |	transaction := self decodeControlWord: aStream.	errorName := self decodeObjectReference: aStream.	response := self decodeObjectReference: aStream.	^RsrDeliverResponse new		transaction: transaction;		errorName: errorName;		response: response;		yourself
%



set class RsrDecoder

method:
decodeSendMessage: aStream	| transaction argCount receiverOID receiver selector arguments |	transaction := self decodeControlWord: aStream.	argCount := self decodeControlWord: aStream.	receiverOID := self decodeControlWord: aStream.	receiver := registry at: receiverOID ifAbsent: [^self signalUnknownOID].	selector := self decodeObjectReference: aStream.	arguments := (1 to: argCount) collect: [:each | self decodeObjectReference: aStream].	^RsrSendMessage		transaction: transaction		receiver: receiver		selector: selector		arguments: arguments
%



set class RsrDecoder

method:
decodeOrderedCollection: aStream	| size oc |	size := self decodeControlWord: aStream.	oc := OrderedCollection new: size.	size timesRepeat: [oc add: (self decodeObjectReference: aStream)].	^oc
%



set class RsrDecoder

method:
decodeDateTime: aStream	| microseconds |	microseconds := self decodeControlWord: aStream.	^RsrDateTimeInterface fromMicroseconds: microseconds
%



set class RsrDecoder

method:
registry: anRsrRegistry	registry := anRsrRegistry
%



set class RsrDecoder

method:
decodeImmediateInteger: aStream	| length bytes |	length := self decodeControlWord: aStream.	bytes := aStream next: length.	^self bytesAsInteger: bytes
%



set class RsrDecoder

method:
decodeSymbol: aStream	^(self decodeString: aStream) asSymbol
%



set class RsrDecoder

method:
connection	^connection
%



set class RsrDecoder

method:
decodeObject: aStream	| objectType |	objectType := self decodeControlWord: aStream.	objectType = self serviceType		ifTrue: [^self decodeService: aStream].	^self error: 'Invalid object'
%



set class RsrDecoder

method:
signalUnknownOID	RsrUnknownOID signal
%



set class RsrDecoder

method:
decodeCommand: aStream	"Decode an object from the stream"	| command |	command := self decodeControlWord: aStream.	command == self retainObjectCommand		ifTrue: [^RsrRetainObject object: (self decodeObject: aStream)].	command == self sendMessageCommand		ifTrue: [^self decodeSendMessage: aStream].	command == self deliverResponseCommand		ifTrue: [^self decodeDeliverResponse: aStream].	command == self releaseObjectsCommand		ifTrue: [^self decodeReleaseObjects: aStream].	self error: 'Invalid message'
%



set class RsrDecoder

method:
decodeNegativeInteger: aStream	^(self decodeImmediateInteger: aStream) negated
%



set class RsrDecoder

method:
connection: aConnection	connection := aConnection
%



set class RsrDecoder

method:
decodeDictionary: aStream	| size dictionary |	size := self decodeControlWord: aStream.	dictionary := Dictionary new: size.	1		to: size		do:			[:i |			dictionary				at: (self decodeObjectReference: aStream)				put: (self decodeObjectReference: aStream)].	^dictionary
%



set class RsrDecoder

method:
decodeImmediateObject: aStream	| type |	type := self decodeControlWord: aStream.	type = self symbolType		ifTrue: [^self decodeSymbol: aStream].	type = self stringType		ifTrue: [^self decodeString: aStream].	type = self positiveIntegerType		ifTrue: [^self decodeImmediateInteger: aStream].	type = self negativeIntegerType		ifTrue: [^self decodeNegativeInteger: aStream].	type = self characterType		ifTrue: [^self decodeCharacter: aStream].	type = self nilType		ifTrue: [^nil].	type = self trueType		ifTrue: [^true].	type = self falseType		ifTrue: [^false].	type = self arrayType		ifTrue: [^self decodeArray: aStream].	type = self dictionaryType		ifTrue: [^self decodeDictionary: aStream].	type = self byteArrayType		ifTrue: [^self decodeByteArray: aStream].	type = self setType		ifTrue: [^self decodeSet: aStream].	type = self orderedCollectionType		ifTrue: [^self decodeOrderedCollection: aStream].	type = self dateTimeType		ifTrue: [^self decodeDateTime: aStream].	self error: 'Invalid immediate type specified (', type printString, ')'
%



set class RsrDecoder

method:
decodeByteArray: aStream	| size |	size := self decodeControlWord: aStream.	^aStream next: size
%

set class RsrAbstractSharedNamespace class

classmethod:
serverClass	^RsrSharedNamespaceServer
%



set class RsrAbstractSharedNamespace class

classmethod:
clientClass	^RsrSharedNamespaceClient
%

set class RsrCommandReader

method:
executeCycle	[self dispatcher dispatch: self nextCommand]		on: RsrConnectionClosed		do:			[:ex |			self log: ex.			self connection disconnected]
%



set class RsrCommandReader

method:
dispatcher	^self connection dispatcher
%



set class RsrCommandReader

method:
nextCommand	^self decoder decodeCommand: self stream
%



set class RsrCommandReader

method:
decoder	^self connection decoder
%

set class RsrCommandDispatcher

method:
priority	^super priority - 1
%



set class RsrCommandDispatcher

method:
dispatch: aCommand	queue nextPut: aCommand
%



set class RsrCommandDispatcher

method:
initialize	super initialize.	queue := SharedQueue new
%



set class RsrCommandDispatcher

method:
executeCycle	queue next executeFor: connection
%

set class RsrAbstractServiceFactory class

classmethod:
serverClass	^RsrServiceFactoryServer
%



set class RsrAbstractServiceFactory class

classmethod:
clientClass	^RsrServiceFactory
%

set class RsrSendMessage class

classmethod:
transaction: aTransactionIdreceiver: aServiceselector: aSelectorarguments: anArray	^self new		transaction: aTransactionId;		receiver: aService;		selector: aSelector;		arguments: anArray;		yourself
%

set class RsrSendMessage

method:
sendOver: aConnection	| analysis promise |	analysis := RsrRetainAnalysis		roots: self roots		connection: aConnection.	analysis perform.	retainList := analysis retainCommands.	self encodeUsing: aConnection encoder.	promise := RsrPromise new.	aConnection promises		at: transaction		put: promise.	aConnection commandWriter enqueue: self.	^promise
%



set class RsrSendMessage

method:
receiver	^ receiver
%



set class RsrSendMessage

method:
arguments	^ arguments
%



set class RsrSendMessage

method:
transaction	^ transaction
%



set class RsrSendMessage

method:
writeUsing: aCommandWriter	retainList do: [:each | each writeUsing: aCommandWriter].	aCommandWriter write: encoding
%



set class RsrSendMessage

method:
selector: anObject	selector := anObject
%



set class RsrSendMessage

method:
roots	^(Array with: receiver with: selector) ,  arguments
%



set class RsrSendMessage

method:
encodeUsing: anEncoder	encoding := anEncoder encodeSendMessage: self
%



set class RsrSendMessage

method:
selector	^ selector
%



set class RsrSendMessage

method:
transaction: anObject	transaction := anObject
%



set class RsrSendMessage

method:
arguments: anObject	arguments := anObject
%



set class RsrSendMessage

method:
receiver: anObject	receiver := anObject
%



set class RsrSendMessage

method:
executeFor: aConnection	| result response |	[result := receiver		perform: selector		withArguments: arguments.	aConnection objectCache reset.	response := RsrDeliverResponse		transaction: transaction		response: result.	response sendOver: aConnection]		on: Error		do: [:ex | (RsrDeliverResponse transaction: transaction error: ex) sendOver: aConnection]
%

set class RsrSharedNamespaceServer

method:
rsrConnection: aConnection	super rsrConnection: aConnection.	namespace := aConnection sharedNamespace.	namespace server: self
%



set class RsrSharedNamespaceServer

method:
removeKey: aKey	^namespace primRemoveKey: aKey
%



set class RsrSharedNamespaceServer

method:
at: aKeyput: aValue	^namespace		primAt: aKey		put: aValue
%

set class RsrSharedNamespaceClient

method:
removeKey: aKey	^remoteSelf removeKey: aKey
%



set class RsrSharedNamespaceClient

method:
at: aKeyput: aValue	^remoteSelf		at: aKey		put: aValue
%

set class RsrServiceFactory

method:
serviceFor: aResponsibility	| instance |	instance := (RsrClassResolver classNamed: aResponsibility)		rsrId: rsrConnection oidSpigot next		rsrConnection: rsrConnection.	instance synchronize.	^instance
%