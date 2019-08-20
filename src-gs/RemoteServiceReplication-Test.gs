run
RsrObject
	subclass: #RsrMockRegistry
	instVarNames: #(#objects #idSpigot)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrSocketPair
	instVarNames: #(#firstSocket #secondSocket)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObject
	subclass: #RsrMockConnection
	instVarNames: #(#forwarderClass #lastMessage #registry #idSpigot)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrForwarder
	subclass: #RsrMockForwarder
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrService
	subclass: #RsrAbstractTestService
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrService
	subclass: #RsrServiceAbstractNoInstVars
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCodec
	subclass: #RsrMockEncoder
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrService
	subclass: #RsrServiceAbstractAllDataObjects
	instVarNames: #(#positiveSmallInteger #negativeSmallInteger #integerZero #string)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrService
	subclass: #RsrAbstractValueHolderService
	instVarNames: #(#value)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrService
	subclass: #RsrServiceAbstractReferenceService
	instVarNames: #(#service)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrRetainAnalysisTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrCodecTest
	instVarNames: #(#registry #decoder)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrServiceAbstractAllDataObjects
	subclass: #RsrServerAllDataObjects
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrSocketStreamTestCase
	instVarNames: #(#serverStream #clientStream)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrServiceAbstractAllDataObjects
	subclass: #RsrClientAllDataObjects
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrPromiseTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrAbstractValueHolderService
	subclass: #RsrValueHolderServer
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrObjectTestCase
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrServiceAbstractReferenceService
	subclass: #RsrServerReferenceService
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrAbstractValueHolderService
	subclass: #RsrValueHolderClient
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrServiceAbstractReferenceService
	subclass: #RsrClientReferenceService
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrNumericSpigotTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrAbstractTestService
	subclass: #RsrServerTestService
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrForwarderTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrServiceAbstractNoInstVars
	subclass: #RsrServerNoInstVars
	instVarNames: #(#marker)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrAbstractTestService
	subclass: #RsrClientTestService
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrServiceAbstractNoInstVars
	subclass: #RsrClientNoInstVars
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrSystemTestCase
	instVarNames: #(#connectionA #connectionB)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCodecTest
	subclass: #RsrDecoderTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrObjectTestCase
	subclass: #RsrServiceTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrSystemTestCase
	subclass: #RsrServiceRegistryTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrNumericSpigotTest
	subclass: #RsrThreadSafeNumericSpigotTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrSystemTestCase
	subclass: #RsrMessageSendingTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrSystemTestCase
	subclass: #RsrLifetimeTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrSystemTestCase
	subclass: #RsrEvaluationClientTest
	instVarNames: #(#service)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrCodecTest
	subclass: #RsrEncoderTest
	instVarNames: #(#connection)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%





set class RsrMockRegistry

method:
objects	^objects
%



set class RsrMockRegistry

method:
initialize	super initialize.	objects := OrderedCollection new
%



set class RsrMockRegistry

method:
remember: anRsrObject	self objects add: anRsrObject
%



set class RsrMockRegistry

method:
newId	^idSpigot next
%



set class RsrMockRegistry

method:
connection	^RsrMockConnection new
%



set class RsrMockRegistry

method:
objects: anObject	objects := anObject
%



set class RsrMockRegistry

method:
register: anRsrObject	self objects add: anRsrObject
%



set class RsrMockRegistry

method:
retain: anRsrObject	self objects add: anRsrObject
%

set class RsrSocketPair class

classmethod:
listenPort	^64455
%



set class RsrSocketPair class

classmethod:
timeout	^2
%



set class RsrSocketPair class

classmethod:
firstSocket: firstSocketsecondSocket: secondSocket	^super new		firstSocket: firstSocket;		secondSocket: secondSocket;		yourself
%



set class RsrSocketPair class

classmethod:
new	| listener firstSocket secondSocket |	listener := RsrSocket new.	secondSocket := RsrSocket new.	listener listenOn: self listenPort.	secondSocket		connectTo: self listenPort		on: '127.0.0.1'.	firstSocket := listener accept.	listener close.	(firstSocket isConnected and: [secondSocket isConnected])		ifFalse: [self error: 'Failed to create socket pair'].	^self		firstSocket: firstSocket		secondSocket: secondSocket
%

set class RsrSocketPair

method:
firstSocket	^ firstSocket
%



set class RsrSocketPair

method:
secondSocket: anObject	secondSocket := anObject
%



set class RsrSocketPair

method:
firstSocket: anObject	firstSocket := anObject
%



set class RsrSocketPair

method:
secondStream	^RsrSocketStream on: secondSocket
%



set class RsrSocketPair

method:
secondSocket	^ secondSocket
%



set class RsrSocketPair

method:
close	firstSocket close.	secondSocket close
%



set class RsrSocketPair

method:
firstStream	^RsrSocketStream on: firstSocket
%

set class RsrMockConnection class

classmethod:
forwarderClass: aClass 	^self new		forwarderClass: aClass;		yourself
%

set class RsrMockConnection

method:
lastMessage	^[lastMessage]		ensure: [lastMessage := nil]
%



set class RsrMockConnection

method:
forwarderClass	^forwarderClass ifNil: [RsrMockForwarder]
%



set class RsrMockConnection

method:
encoder	^RsrMockEncoder new
%



set class RsrMockConnection

method:
registry	^registry ifNil: [registry := RsrRegistry new]
%



set class RsrMockConnection

method:
releaseOid: anInteger
%



set class RsrMockConnection

method:
rsrForwarderClass	^self forwarderClass
%



set class RsrMockConnection

method:
rsrSendMessage: aMessageto: anRsrObject	lastMessage := RsrSendMessage		transaction: 1		receiver: anRsrObject		selector: aMessage selector		arguments: aMessage arguments.	^nil
%



set class RsrMockConnection

method:
forwarderClass: aClass	forwarderClass := aClass
%



set class RsrMockConnection

method:
oidSpigot	^idSpigot ifNil: [idSpigot := RsrThreadSafeNumericSpigot naturals]
%

set class RsrMockForwarder

method:
rsrObject	^rsrObject
%

set class RsrAbstractTestService class

classmethod:
serverClass	^RsrServerTestService
%



set class RsrAbstractTestService class

classmethod:
clientClass	^RsrClientTestService
%

set class RsrAbstractTestService

method:
remoteSelf	^remoteSelf
%

set class RsrServiceAbstractNoInstVars class

classmethod:
serverClass	^RsrServerNoInstVars
%



set class RsrServiceAbstractNoInstVars class

classmethod:
clientClass	^RsrClientNoInstVars
%

set class RsrServiceAbstractNoInstVars

method:
sendReturnArgument: anObject	^remoteSelf returnArgument: anObject
%



set class RsrServiceAbstractNoInstVars

method:
returnArgument: anObject	^anObject
%

set class RsrMockEncoder

method:
encodeRetainObject: anObject	^ByteArray new
%



set class RsrMockEncoder

method:
encodeObject: anObject	^ByteArray new
%



set class RsrMockEncoder

method:
encode: anObject	^RsrRetainObject		object: anObject		encoding: ByteArray new
%

set class RsrServiceAbstractAllDataObjects class

classmethod:
serverClass	^RsrServerAllDataObjects
%



set class RsrServiceAbstractAllDataObjects class

classmethod:
clientClass	^RsrClientAllDataObjects
%

set class RsrServiceAbstractAllDataObjects

method:
initialize	super initialize.	negativeSmallInteger := -1.	integerZero := 0.	positiveSmallInteger := 1.	string := 'hello, world'
%

set class RsrAbstractValueHolderService class

classmethod:
value: anRsrObject	^self new		value: anRsrObject;		yourself
%



set class RsrAbstractValueHolderService class

classmethod:
serverClass	^RsrValueHolderServer
%



set class RsrAbstractValueHolderService class

classmethod:
clientClass	^RsrValueHolderClient
%

set class RsrAbstractValueHolderService

method:
value: anObject	value := anObject.	self synchronize
%



set class RsrAbstractValueHolderService

method:
value	^value
%

set class RsrServiceAbstractReferenceService class

classmethod:
service: aService	^self new		service: aService;		yourself
%



set class RsrServiceAbstractReferenceService class

classmethod:
serverClass	^RsrServerReferenceService
%



set class RsrServiceAbstractReferenceService class

classmethod:
clientClass	^RsrClientReferenceService
%

set class RsrServiceAbstractReferenceService

method:
service	^ service
%



set class RsrServiceAbstractReferenceService

method:
service: anObject	service := anObject
%

set class RsrRetainAnalysisTest

method:
testServiceNoInstVars	| client registry analysis expected |	client := RsrClientNoInstVars new.	registry := RsrMockRegistry new.	analysis := RsrRetainAnalysis		roots: (Array with: client)		connection: RsrMockConnection new.	analysis perform.	expected := OrderedCollection		with: (RsrRetainObject object: client encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self assert: client isMirrored
%



set class RsrRetainAnalysisTest

method:
testNewServicesInDictionary	"Ensure a new service in a collection is properly tagged"	| key value dictionary analysis expected |	key := RsrServerNoInstVars new.	value := RsrServerNoInstVars new.	dictionary := Dictionary new		at: key put: value;		yourself.	analysis := RsrRetainAnalysis		roots: (Array with: dictionary)		connection: RsrMockConnection new.	analysis perform.	expected := OrderedCollection		with: (RsrRetainObject object: key encoding: ByteArray new)		with: (RsrRetainObject object: value encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self		assert: key isMirrored;		assert: value isMirrored
%



set class RsrRetainAnalysisTest

method:
testServiceReferencingAnotherService	"While this code is structurally similar to #testClientNoInstVars, it ensures	that Data Objects are actually encoded in-line."	| referencedService client registry analysis expected |	referencedService := RsrClientNoInstVars new.	client := RsrClientReferenceService service: referencedService.	registry := RsrMockRegistry new.	analysis := RsrRetainAnalysis		roots: (Array with: client)		connection: RsrMockConnection new.	analysis perform.	expected := OrderedCollection		with: (RsrRetainObject object: referencedService encoding: ByteArray new)		with: (RsrRetainObject object: client encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self		assert: client isMirrored;		assert: referencedService isMirrored
%



set class RsrRetainAnalysisTest

method:
testServiceAllDataObjects	"While this code is structurally similar to #testClientNoInstVars, it ensures	that Data Objects are actually encoded in-line."	| client registry analysis expected |	client := RsrClientAllDataObjects new.	registry := RsrMockRegistry new.	analysis := RsrRetainAnalysis		roots: (Array with: client)		connection: RsrMockConnection new.	analysis perform.	expected := OrderedCollection		with: (RsrRetainObject object: client encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self assert: client isMirrored
%



set class RsrRetainAnalysisTest

method:
testNewServiceInArray	"Ensure a new service in a collection is properly tagged"	| service analysis expected |	service := RsrServerNoInstVars new.	analysis := RsrRetainAnalysis		roots: (Array with: (Array with: service))		connection: RsrMockConnection new.	analysis perform.	expected := OrderedCollection		with: (RsrRetainObject object: service encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self assert: service isMirrored
%



set class RsrRetainAnalysisTest

method:
testServiceWithCycle	"Cycles are disallowed for our POC. Perhaps they will get added later?"	| rootClient referencedClient registry analysis |	rootClient := RsrClientReferenceService new.	referencedClient := RsrClientReferenceService service: rootClient.	rootClient service: referencedClient.	registry := RsrMockRegistry new.	analysis := RsrRetainAnalysis		roots: (Array with: rootClient)		connection: RsrMockConnection new.	self		should: [analysis perform]		raise: RsrCycleDetected
%

set class RsrCodecTest class

classmethod:
isAbstract	^self == RsrCodecTest
%

set class RsrCodecTest

method:
testDateTime	| dt encoding |	dt := DateAndTime unixEpoch.	encoding :=		#[0 0 0 0 0 0 0 0],		#[0 0 0 0 0 0 0 14],		#[0 0 0 0 0 0 0 0].	self		verifyImmediate: dt		encoding: encoding.	dt := DateAndTime fromString: '2019-07-09T10:16:02.657612-07:00'.	encoding :=		#[0 0 0 0 0 0 0 0],		#[0 0 0 0 0 0 0 14],		#[0 5 141 66 183 23 33 76].	self		verifyImmediate: dt		encoding: encoding.	dt := DateAndTime fromString: '1969-12-31T23:59:59-00:00'.	encoding :=		#[0 0 0 0 0 0 0 0],		#[0 0 0 0 0 0 0 14],		#[255 255 255 255 255 240 189 192].	self		verifyImmediate: dt		encoding: encoding.	dt := DateAndTime fromString: '1954-06-07T14:59:17.432512-07:00'.	encoding :=		#[0 0 0 0 0 0 0 0],		#[0 0 0 0 0 0 0 14],		#[255 254 65 47 130 160 240 192].	self		verifyImmediate: dt		encoding: encoding
%



set class RsrCodecTest

method:
testString	| encoding |	encoding :=		#[0 0 0 0 0 0 0 0], "OID = 0"		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"		#[0 0 0 0 0 0 0 13], "length"		#[103 101 110 101 114 105 99 83 116 114 105 110 103].	 "genericString"	self		verifyImmediate: 'genericString'		encoding: encoding
%



set class RsrCodecTest

method:
genericSymbol	^#genericSymbol
%



set class RsrCodecTest

method:
testSet	| set encoding result |	set := Set new.	encoding :=		#[0 0 0 0 0 0 0 0], "OID"		#[0 0 0 0 0 0 0 11], "Set"		#[0 0 0 0 0 0 0 0]. "0 elements"	self		verifyImmediate: set		encoding: encoding.	set := Set		with: true		with: nil.	encoding := self encodeReferenceOf: set.	result := self decoder decodeObjectReference: encoding readStream.	self		assert: result		equals: set.	self		deny: result		identicalTo: set.	"self hack: 'Hashed collections do not have an ordering'.	encoding :=		#[0 0 0 0 0 0 0 0], ""OID""		#[0 0 0 0 0 0 0 11], ""Set""		#[0 0 0 0 0 0 0 2], ""2 elements""		#[0 0 0 0 0 0 0 0], ""true""		#[0 0 0 0 0 0 0 7],		#[0 0 0 0 0 0 0 0], ""nil""		#[0 0 0 0 0 0 0 6].	self		verifyImmediate: set		encoding: encoding"
%



set class RsrCodecTest

method:
verifyImmediate: anImmediateObjectencoding: encoding	self subclassResponsibility
%



set class RsrCodecTest

method:
serviceNoInstVarsEncoding	^#[0 0 0 0 0 0 0 0], "type"	#[0 0 0 0 0 0 0 1], "rootService's OID = 1"	#[0 0 0 0 0 0 0 0], "Inst Var Count"	#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"	#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"	#[0 0 0 0 0 0 0 19], "Length of UTF-8 encoded bytes"	#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115] "#RsrServerNoInstVars"
%



set class RsrCodecTest

method:
encoder	^RsrEncoder new
%



set class RsrCodecTest

method:
testSymbol	self		verifyImmediate: self genericSymbol		encoding: self genericSymbolEncoding
%



set class RsrCodecTest

method:
testArray	| array encoding |	array := Array		with: self genericSymbol		with: 5		with: nil.	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate Object OID"		#[0 0 0 0 0 0 0 9], "Array type"		#[0 0 0 0 0 0 0 3], "3 elements"		self genericSymbolEncoding, "Generic Symbol"		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 3], "Positive Integer"		#[0 0 0 0 0 0 0 1], "num bytes"		#[5], "5"		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 6].	self		verifyImmediate: array		encoding: encoding.	array := Array new.	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 9], "Array type"		#[0 0 0 0 0 0 0 0].	self		verifyImmediate: array		encoding: encoding
%



set class RsrCodecTest

method:
testControlWord		self		verifyControlWord: 0		encoding: #[0 0 0 0 0 0 0 0].	self		verifyControlWord: 1		encoding: #[0 0 0 0 0 0 0 1].	self		verifyControlWord: -1		encoding: #[255 255 255 255 255 255 255 255].	self		verifyControlWord: (2 raisedTo: 63) - 1		encoding: #[127 255 255 255 255 255 255 255].	self		verifyControlWord: (2 raisedTo: 63) negated		encoding: #[128 0 0 0 0 0 0 0]
%



set class RsrCodecTest

method:
rootServiceEncoding	^#[0 0 0 0 0 0 0 0], "type"	#[0 0 0 0 0 0 0 1], "rootService's OID = 1"	#[0 0 0 0 0 0 0 1], "Inst Var Count"	#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"	#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"	#[0 0 0 0 0 0 0 25], "Length of UTF-8 encoded bytes"	#[82 115 114 83 101 114 118 101 114 82 101 102 101 114 101 110 99 101 83 101 114 118 105 99 101],	#[0 0 0 0 0 0 0 2]. "#RsrServerReferenceService"
%



set class RsrCodecTest

method:
genericSymbolEncoding	^#[0 0 0 0 0 0 0 0], "OID = 0"	#[0 0 0 0 0 0 0 1], "Immediate Type = 1"	#[0 0 0 0 0 0 0 13], "Length of UTF-8 data"	#[103 101 110 101 114 105 99 83 121 109 98 111 108]	"#genericSymbol"
%



set class RsrCodecTest

method:
referencedServiceEncoding	^#[0 0 0 0 0 0 0 0], "type"	#[0 0 0 0 0 0 0 2], "referencedService's OID = 2"	#[0 0 0 0 0 0 0 0], "Inst Var Count"	#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"	#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"	#[0 0 0 0 0 0 0 19], "Length of UTF-8 encoded bytes"	#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"
%



set class RsrCodecTest

method:
testOrderedCollection	| oc encoding |	oc := OrderedCollection new.	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 12], "OrderedCollection type"		#[0 0 0 0 0 0 0 0].	self		verifyImmediate: oc		encoding: encoding.	oc := OrderedCollection		with: self genericSymbol		with: 5		with: nil.	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate Object OID"		#[0 0 0 0 0 0 0 12], "OrderedCollection type"		#[0 0 0 0 0 0 0 3], "3 elements"		self genericSymbolEncoding, "Generic Symbol"		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 3], "Positive Integer"		#[0 0 0 0 0 0 0 1], "num bytes"		#[5], "5"		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 6].	self		verifyImmediate: oc		encoding: encoding
%



set class RsrCodecTest

method:
testDictionary	| dictionary encoding result |	dictionary := Dictionary new.	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate Object OID"		#[0 0 0 0 0 0 0 13], "Dictionary type"		#[0 0 0 0 0 0 0 0]. "0 associations"	self		verifyImmediate: dictionary		encoding: encoding.	dictionary := Dictionary new		at: 1 put: self genericSymbol;		at: nil put: true;		yourself.	encoding := self encodeReferenceOf: dictionary.	result := self decoder decodeObjectReference: encoding readStream.	self		assert: result		equals: dictionary.	self		deny: result		identicalTo: dictionary.	"self hack: 'Order is not guaranteed in a dictionary'.	encoding :=		#[0 0 0 0 0 0 0 0], ""Immediate OID""		#[0 0 0 0 0 0 0 13], ""Dictionary Type""		#[0 0 0 0 0 0 0 2], ""Two assocs""		#[0 0 0 0 0 0 0 0], ""nil""		#[0 0 0 0 0 0 0 6],		#[0 0 0 0 0 0 0 0], ""true""		#[0 0 0 0 0 0 0 7],		#[0 0 0 0 0 0 0 0], ""Integer 1""		#[0 0 0 0 0 0 0 3],		#[0 0 0 0 0 0 0 1],		#[1],		self genericSymbolEncoding.	self		verifyImmediate: dictionary		encoding: encoding"
%



set class RsrCodecTest

method:
verifyControlWord: anIntegerencoding: bytes	self subclassResponsibility
%



set class RsrCodecTest

method:
testByteArray	| bytes encoding |	bytes := #[].	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate Object OID"		#[0 0 0 0 0 0 0 10], "ByteArray type"		#[0 0 0 0 0 0 0 0], "size"		bytes.	self		verifyImmediate: bytes		encoding: encoding.	bytes := #[1 2 3 4 5].	encoding :=		#[0 0 0 0 0 0 0 0],		#[0 0 0 0 0 0 0 10],		#[0 0 0 0 0 0 0 5],		bytes.	self		verifyImmediate: bytes		encoding: encoding
%



set class RsrCodecTest

method:
decoder	^decoder ifNil: [decoder := RsrDecoder registry: (registry := RsrRegistry new) connection: RsrMockConnection new]
%



set class RsrCodecTest

method:
encodeReferenceOf: anObject	^ByteArray streamContents: [:stream | self encoder encodeReferenceOf: anObject onto: stream]
%

set class RsrSocketStreamTestCase

method:
listenPort	^47856
%



set class RsrSocketStreamTestCase

method:
setUp	super setUp.	self initializeStreams
%



set class RsrSocketStreamTestCase

method:
tearDown	serverStream close.	clientStream close.	super tearDown
%



set class RsrSocketStreamTestCase

method:
testClose	serverStream close.	self		deny: serverStream isConnected;		assert: clientStream isConnected.	self		assert: clientStream next		equals: nil.	self		deny: clientStream isConnected
%



set class RsrSocketStreamTestCase

method:
initializeStreams	| listener client server |	listener := RsrSocket new.	client := RsrSocket new.	listener listenOn: self listenPort.	client connectTo: self listenPort on: '127.0.0.1'.	server := listener accept.	listener close.	serverStream := RsrSocketStream on: server.	clientStream := RsrSocketStream on: client
%

set class RsrPromiseTest

method:
testFulfillment	| promise expected semaphore |	promise := RsrPromise new.	expected := Object new.	[(Delay forSeconds: 1) wait.	promise fulfill: expected] fork.	self		assert: promise value		identicalTo: expected.	promise := RsrPromise new.	semaphore := Semaphore new.	[promise fulfill: expected.	semaphore signal] fork.	semaphore wait.	self		assert: promise value		identicalTo: expected
%



set class RsrPromiseTest

method:
testError	| promise semaphore |	promise := RsrPromise new.	[(Delay forSeconds: 1) wait.	promise error: Error new] fork.	self		should: [promise value]		raise: Error.	promise := RsrPromise new.	semaphore := Semaphore new.	[promise error: Error new.	semaphore signal] fork.	self		should: [promise value]		raise: Error
%

set class RsrObjectTestCase class

classmethod:
isAbstract	^self == RsrObjectTestCase
%

set class RsrObjectTestCase

method:
newInstance	self subclassResponsibility
%



set class RsrObjectTestCase

method:
testInitialization	| instance |	instance := self newInstance.	self		assert: instance isMirrored		equals: false.	self		assert: instance rsrId		equals: nil.	self		assert: instance rsrConnection		equals: nil
%



set class RsrObjectTestCase

method:
testIsMirrored	| instance |	instance := self newInstance.	self deny: instance isMirrored.	instance		rsrId: 1;		rsrConnection: RsrMockConnection new.	self assert: instance isMirrored
%

set class RsrNumericSpigotTest

method:
spigotClass	^RsrNumericSpigot
%



set class RsrNumericSpigotTest

method:
testDefault	| spigot |	spigot := self spigotClass new.	self		assert: spigot next		equals: 0.	self		assert: spigot next		equals: 1
%



set class RsrNumericSpigotTest

method:
testNegativeStep	| spigot |	spigot := self spigotClass		start: 0		step: -1.	self		assert: spigot next		equals: 0.	self		assert: spigot next		equals: -1.	self		assert: spigot next		equals: -2
%



set class RsrNumericSpigotTest

method:
testNext	| spigot |	spigot := self spigotClass naturals.	self		assert: (Array with: 1 with: 2 with: 3)		equals: (spigot next: 3)
%



set class RsrNumericSpigotTest

method:
testNaturals	| spigot |	spigot := self spigotClass naturals.	self		assert: spigot next		equals: 1.	self		assert: spigot next		equals: 2
%



set class RsrNumericSpigotTest

method:
testFloat	| spigot |	spigot := self spigotClass		start: 0		step: 0.5.	self		assert: spigot next		equals: 0.	self		assert: spigot next		equals: 0.5.	self		assert: spigot next		equals: 1.0.
%

set class RsrForwarderTest

method:
testForwarding	| rsrObject id remoteInterface forwarder message |	rsrObject := RsrClientTestService new.	id := 1.	remoteInterface := RsrMockConnection forwarderClass: RsrForwarder.	rsrObject		rsrId: id;		rsrConnection: remoteInterface.	forwarder := rsrObject remoteSelf.	forwarder		arg1: 15		arg2: 42.	message := remoteInterface lastMessage.	self		assert: message transaction		equals: 1.	self		assert: message receiver		equals: rsrObject.	self		assert: message selector		equals: #arg1:arg2:.	self		assert: message arguments		equals: #(15 42).
%

set class RsrServerNoInstVars

method:
setMarker	marker := true
%



set class RsrServerNoInstVars

method:
returnNewServiceInArray	^Array with: RsrServerTestService new
%



set class RsrServerNoInstVars

method:
marker	^marker ifNil: [false]
%



set class RsrServerNoInstVars

method:
unaryReturn42	^42
%



set class RsrServerNoInstVars

method:
returnTestSymbol	^#testSymbol
%



set class RsrServerNoInstVars

method:
returnNewService	^RsrServerTestService new
%

set class RsrClientNoInstVars

method:
sendReturnTestSymbol	^remoteSelf returnTestSymbol
%



set class RsrClientNoInstVars

method:
sendSetMarker	^remoteSelf setMarker
%



set class RsrClientNoInstVars

method:
sendReturnNewService	^remoteSelf returnNewService
%



set class RsrClientNoInstVars

method:
sendUnaryReturn42Message	^remoteSelf unaryReturn42
%



set class RsrClientNoInstVars

method:
sendReturnNewServiceInArray	^remoteSelf returnNewServiceInArray
%

set class RsrSystemTestCase class

classmethod:
isAbstract	^self == RsrSystemTestCase
%

set class RsrSystemTestCase

method:
sharedNamespaceA	^connectionA sharedNamespace
%



set class RsrSystemTestCase

method:
setUp	| socketPair semaphore |	super setUp.	socketPair := RsrSocketPair new.	connectionA := RsrConnection		socket: socketPair firstSocket		transactionSpigot: RsrThreadSafeNumericSpigot naturals		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1).	connectionB := RsrConnection		socket: socketPair secondSocket		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated		oidSpigot: RsrThreadSafeNumericSpigot naturals negated.	semaphore := Semaphore new.	[connectionA start.	semaphore signal] fork.	[connectionB start.	semaphore signal] fork.	semaphore wait; wait
%



set class RsrSystemTestCase

method:
sharedNamespaceB	^connectionB sharedNamespace
%



set class RsrSystemTestCase

method:
tearDown	connectionA ifNotNil: [:conn | conn close].	connectionB ifNotNil: [:conn | conn close].	connectionA := connectionB := nil.	super tearDown
%

set class RsrDecoderTest

method:
testSendMessage	| service encoding command |	service := RsrServerNoInstVars		rsrId: 1		rsrConnection: RsrMockConnection new.	self decoder.	service registerWith: registry.	encoding :=		#[0 0 0 0 0 0 0 1], "SendMessage Command"		#[0 0 0 0 0 0 0 1], "Transaction ID"		#[0 0 0 0 0 0 0 0], "Argument Count"		#[0 0 0 0 0 0 0 1], "Receiver OID"		#[0 0 0 0 0 0 0 0], "Selector Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 8], "Length of UTF-8 bytes"		#[114 101 116 117 114 110 52 50]. "#return42"	command := self decoder decodeCommand: encoding readStream.	self		assert: command class		equals: RsrSendMessage.	self		assert: command transaction		equals: 1.	self		assert: command receiver		identicalTo: service.	self		assert: command selector		identicalTo: #return42.	self		assert: command arguments		equals: #()
%



set class RsrDecoderTest

method:
testServiceNoInstVars	| decodedService |	decodedService := self decode: self serviceNoInstVarsEncoding.	self		assert: decodedService class		equals: RsrServerNoInstVars.	self		assert: decodedService rsrId		equals: 1
%



set class RsrDecoderTest

method:
assertReference: bytesdecodesTo: expected	| actual |	actual := self decoder decodeObjectReference: bytes readStream.	self		assert: actual		equals: expected
%



set class RsrDecoderTest

method:
testServiceReferenceService	| rootService referencedService |	referencedService := self decode: self referencedServiceEncoding.	self		assert: referencedService class		equals: RsrServerNoInstVars.	self		assert: referencedService rsrId		equals: 2.	rootService := self decode: self rootServiceEncoding.	self		assert: rootService class		equals: RsrServerReferenceService.	self		assert: rootService service		equals: referencedService
%



set class RsrDecoderTest

method:
verifyImmediate: expectedencoding: encoding	| actual |	actual := self decoder decodeObjectReference: encoding readStream.	self		assert: actual		equals: expected
%



set class RsrDecoderTest

method:
testRetainObject	| encoding command |	encoding :=		#[0 0 0 0 0 0 0 0], "RetainObject Command"		#[0 0 0 0 0 0 0 0], "ServiceType Object"		#[0 0 0 0 0 0 0 1], "Service OID"		#[0 0 0 0 0 0 0 0], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 19], "Length of UTF-8 bytes"		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"	command := self decoder decodeCommand: encoding readStream.	self		assert: command object class		equals: RsrServerNoInstVars.	self		assert: command object rsrId		equals: 1.	self assert: (registry includes: command object)
%



set class RsrDecoderTest

method:
testDeliverResponse	| response encoding command |	response := #responseSymbol.	encoding :=		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"		#[0 0 0 0 0 0 0 1], "Transaction Id"		#[0 0 0 0 0 0 0 0], "nil errorName"		#[0 0 0 0 0 0 0 6], "nil errorName"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 14], "Length of UTF-8 bytes"		#[114 101 115 112 111 110 115 101 83 121 109 98 111 108]. "#responseSymbol"	command := self decoder decodeCommand: encoding readStream.	self		assert: command class		equals: RsrDeliverResponse.	self		assert: command transaction		equals: 1.	self		assert: command response		equals: response
%



set class RsrDecoderTest

method:
testServiceDecodeIdentity	"Ensure that decoding an object multiple times results in	a single object getting created."	| firstService secondService |	firstService := self decode: self serviceNoInstVarsEncoding.	secondService := self decode: self serviceNoInstVarsEncoding.	self		assert: firstService		identicalTo: secondService
%



set class RsrDecoderTest

method:
testReleaseObjects	| command encoding |	encoding :=		#[0 0 0 0 0 0 0 3], "ReleaseObjects Command"		#[0 0 0 0 0 0 0 5], "Num OIDS"		#[0 0 0 0 0 0 0 1], "First OID"		#[0 0 0 0 0 0 0 2],		#[0 0 0 0 0 0 0 3],		#[0 0 0 0 0 0 0 4],		#[0 0 0 0 0 0 0 5]. "Last OID"	command := self decoder decodeCommand: encoding readStream.	self		assert: command oids		equals: #(1 2 3 4 5)
%



set class RsrDecoderTest

method:
verifyControlWord: expectedencoding: bytes	| actual |	actual := self decoder decodeControlWord: bytes readStream.	self		assert: actual		equals: expected
%



set class RsrDecoderTest

method:
decode: anObjectBytes	^self decoder decodeObject: anObjectBytes readStream
%

set class RsrServiceTest

method:
newInstance	^RsrClientTestService new
%



set class RsrServiceTest

method:
testHasRemoteSelf	| service |	service := self newInstance.	service		rsrId: 1;		rsrConnection: RsrMockConnection new.	self		deny: service remoteSelf		equals: nil
%

set class RsrServiceRegistryTest

method:
testRegistration	| name service marker actual |	name := 'TestService'.	service := RsrClientNoInstVars new.	marker := Object new.	self sharedNamespaceA		at: name		put: service.	actual := self sharedNamespaceA at: name.	self		assert: actual		identicalTo: actual.	actual := self sharedNamespaceB at: name.	self		assert: actual class		equals: RsrServerNoInstVars.	self		assert: actual rsrId		equals: service rsrId.	self sharedNamespaceB removeKey: name.	self		assert: (self sharedNamespaceA at: name ifAbsent:[marker])		identicalTo: marker.	self		assert: (self sharedNamespaceB at: name ifAbsent:[marker])		identicalTo: marker
%



set class RsrServiceRegistryTest

method:
testCloseWithDanglingObjectInRegistry	"Since the Connection is used in finalization, it cannot hold a strong reference	to objects that should be finalized. The ServiceRegistry may hold onto such a reference.	Ensure objects stored in the ServiceRegistry do not prevent finalization."	| name service marker actual |	name := 'TestService'.	service := RsrClientNoInstVars new.	marker := Object new.	self sharedNamespaceA		at: name		put: service.	actual := self sharedNamespaceA at: name.	self		assert: actual		identicalTo: actual.	actual := self sharedNamespaceB at: name.	self		assert: actual class		equals: RsrServerNoInstVars.	self		assert: actual rsrId		equals: service rsrId
%

set class RsrThreadSafeNumericSpigotTest

method:
spigotClass	^RsrThreadSafeNumericSpigot
%

set class RsrMessageSendingTest

method:
testReturnInvalidObject	| service |	service := connectionA serviceFor: #RsrEvaluationClient.	self		should: [service evaluate: 'Object new']		raise: RsrUnsupportedObject
%



set class RsrMessageSendingTest

method:
testReturnNewServiceInArray	| service array returnedService |	service := connectionA serviceFor: #RsrClientNoInstVars.	array := service sendReturnNewServiceInArray.	self		assert: array size		equals: 1.	returnedService := array first.	self		assert: returnedService class		equals: RsrClientTestService
%



set class RsrMessageSendingTest

method:
testReturnNewService	| service returnedService |	service := connectionA serviceFor: #RsrClientNoInstVars.	returnedService := service sendReturnNewService.	self		assert: returnedService class		equals: RsrClientTestService
%



set class RsrMessageSendingTest

method:
testReturnArgument	| client server arguments |	client := RsrClientNoInstVars new.	self sharedNamespaceA		at: 'TestService'		put: client.	server := self sharedNamespaceB at: 'TestService'.	arguments := OrderedCollection new		addAll: #( #symbol 'string' $h 0 -14 14 18446744073709551616 -18446744073709551616 nil true false ); 		add: (Character codePoint: 16r259F);		add: DateAndTime now;		add: (Dictionary new at: 1 put: 2; yourself);		add: (Set with: 14);		add: #[1 2 3 4];		add: (OrderedCollection with: 42 with: 43);		add: #(1 2 #(nil));		yourself.	arguments		do:			[:each | | result |			result := client sendReturnArgument: each.			self				assert: result				equals: each].	arguments		do:			[:each | | result |			result := server sendReturnArgument: each.			self				assert: result				equals: each].	self		assert: (client sendReturnArgument: arguments)		equals: arguments.	self		assert: (server sendReturnArgument: arguments)		equals: arguments.	self		assert: (client sendReturnArgument: client)		identicalTo: client.	self		assert: (server sendReturnArgument: server)		identicalTo: server
%



set class RsrMessageSendingTest

method:
testSendSetMarker	| service remoteService |	service := RsrClientNoInstVars new.	self sharedNamespaceA		at: 'TestService'		put: service.	remoteService := self sharedNamespaceB at: 'TestService'.	self deny: remoteService marker.	service sendSetMarker.	self assert: remoteService marker
%



set class RsrMessageSendingTest

method:
testRemoteError	| service |	service := connectionA serviceFor: #RsrEvaluationClient.	self		should: [service evaluate: 'Error signal']		raise: Error
%



set class RsrMessageSendingTest

method:
testReturnSymbol	| service |	service := connectionA serviceFor: #RsrClientNoInstVars.	self		assert: service sendReturnTestSymbol		equals: #testSymbol
%



set class RsrMessageSendingTest

method:
testSendInvalidObject	| service |	service := connectionA serviceFor: #RsrClientNoInstVars.	self		should: [service sendReturnArgument: Object new]		raise: RsrUnsupportedObject
%

set class RsrLifetimeTest

method:
testCloseWithDanglingObject	"It is possible that the connection could disconnect between when an object	is received and when the upcoming SendMessage or DeliverResponse message is received.	If this is the case, we coul leak memory due to the caching used to ensure	the object is stored in memory long enough to process the upcoming message.	Test to ensure the object is freed on the connection close."	| service command |	self maximumReclamation.	self assert: RsrClientNoInstVars allInstances isEmpty.	service := RsrServerNoInstVars new.	service		rsrId: 2;		rsrConnection: connectionA.	command := RsrRetainObject object: service.	command		encodeUsing: connectionA encoder;		writeUsing: connectionA commandWriter.	connectionA commandWriter flush.	connectionA close.	connectionB close.	service := command := nil.	self maximumReclamation.	self assert: RsrClientNoInstVars allInstances isEmpty
%



set class RsrLifetimeTest

method:
testRemoteReferenceLifetime	| name valueServiceLocal valueServiceRemote serviceLocal serviceRemote id marker actual |	name := 'ValueService'.	serviceLocal := RsrClientNoInstVars new.	valueServiceLocal := RsrValueHolderClient value: serviceLocal.	self sharedNamespaceA		at: name		put: valueServiceLocal.	valueServiceRemote := self sharedNamespaceB at: name.	serviceRemote := valueServiceRemote value.	id := serviceLocal rsrId.	self		assert: serviceRemote class		equals: RsrServerNoInstVars.	serviceLocal := serviceRemote := nil.	valueServiceRemote value: nil.	self maximumReclamation.	(Delay forSeconds: 1) wait. "Needed to ensure there is time for release to propogate to remote environment."	marker := Object new.	actual := connectionA registry at: id ifAbsent: [marker].	self		assert: actual		equals: marker.	actual := connectionB registry at: id ifAbsent: [marker].	self		assert: actual		equals: marker
%

set class RsrEvaluationClientTest

method:
testEvaluteLiterals	self literals		do:			[:each | | result |			result := service evaluate: each printString.			self				assert: result				equals: each]
%



set class RsrEvaluationClientTest

method:
setUp	super setUp.	service := connectionA serviceFor: #RsrEvaluationClient
%



set class RsrEvaluationClientTest

method:
literals	^#( nil true false -1 0 1 )
%



set class RsrEvaluationClientTest

method:
testEvaluteService	| result |	result := service evaluate: 'RsrEvaluationServer new'.	self		assert: result class		equals: RsrEvaluationClient
%

set class RsrEncoderTest

method:
testSendMessage	| service command expectedEncoding |	service := RsrClientNoInstVars new.	self register: service.	command := RsrSendMessage		transaction: 1		receiver: service		selector: #return42		arguments: #().	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 1], "SendMessage Command"		#[0 0 0 0 0 0 0 1], "Transaction ID"		#[0 0 0 0 0 0 0 0], "Argument Count"		#[0 0 0 0 0 0 0 1], "Receiver OID"		#[0 0 0 0 0 0 0 0], "Selector Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 8], "Length of UTF-8 bytes"		#[114 101 116 117 114 110 52 50]. "#return42"	self		assert: command encoding		equals: expectedEncoding
%



set class RsrEncoderTest

method:
testServiceNoInstVars	| rootService encodedBytes expectedEncoding |	rootService := RsrClientNoInstVars new.	self register: rootService.	encodedBytes := self encoder encodeObject: rootService.	expectedEncoding := self serviceNoInstVarsEncoding.	self		assert: encodedBytes		equals: expectedEncoding
%



set class RsrEncoderTest

method:
testServiceReferenceService	| rootService referencedService encodedObject expectedEncoding |	referencedService := RsrClientNoInstVars new.	rootService := RsrClientReferenceService service: referencedService.	self		register: rootService;		register: referencedService.	encodedObject := self encoder encodeObject: rootService.	expectedEncoding :=		#[0 0 0 0 0 0 0 0], "type"		#[0 0 0 0 0 0 0 1], "rootService's OID = 1"		#[0 0 0 0 0 0 0 1], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"		#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"		#[0 0 0 0 0 0 0 25], "Length of UTF-8 encoded bytes"		#[82 115 114 83 101 114 118 101 114 82 101 102 101 114 101 110 99 101 83 101 114 118 105 99 101],		#[0 0 0 0 0 0 0 2]. "#RsrServerReferenceService"	self		assert: encodedObject		equals: expectedEncoding.	encodedObject := self encoder encodeObject: referencedService.	expectedEncoding :=		#[0 0 0 0 0 0 0 0], "type"		#[0 0 0 0 0 0 0 2], "referencedService's OID = 2"		#[0 0 0 0 0 0 0 0], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"		#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"		#[0 0 0 0 0 0 0 19], "Length of UTF-8 encoded bytes"		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"	self		assert: encodedObject		equals: expectedEncoding
%



set class RsrEncoderTest

method:
verifyImmediate: anObjectencoding: expected	| actual |	actual := ByteArray streamContents: [:stream | self encoder encodeReferenceOf: anObject onto: stream].	self		assert: actual		equals: expected
%



set class RsrEncoderTest

method:
setUp	super setUp.	registry := RsrMockRegistry new.	connection := RsrMockConnection new
%



set class RsrEncoderTest

method:
testRetainObject	| service command expectedEncoding |	service := RsrClientNoInstVars new.	self register: service.	command := RsrRetainObject object: service.	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 0], "RetainObject Command"		#[0 0 0 0 0 0 0 0], "ServiceType Object"		#[0 0 0 0 0 0 0 1], "Service OID"		#[0 0 0 0 0 0 0 0], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 19], "Length of UTF-8 bytes"		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"	self		assert: command encoding		equals: expectedEncoding
%



set class RsrEncoderTest

method:
testDeliverResponse	| response command expectedEncoding |	response := #responseSymbol.	command := RsrDeliverResponse		transaction: 1		response: response.	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"		#[0 0 0 0 0 0 0 1], "Transaction Id"		#[0 0 0 0 0 0 0 0], "nil errorName"		#[0 0 0 0 0 0 0 6], "nil errorName"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 14], "Length of UTF-8 bytes"		#[114 101 115 112 111 110 115 101 83 121 109 98 111 108]. "#responseSymbol"	self		assert: command encoding		equals: expectedEncoding
%



set class RsrEncoderTest

method:
register: aService	aService		rsrId: connection oidSpigot next;		rsrConnection: connection.	registry retain: aService
%



set class RsrEncoderTest

method:
testReleaseObjects	| command expectedEncoding |	command := RsrReleaseObjects oids: #(1 2 3 4 5).	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 3], "ReleaseObjects Command"		#[0 0 0 0 0 0 0 5], "Num OIDS"		#[0 0 0 0 0 0 0 1], "First OID"		#[0 0 0 0 0 0 0 2],		#[0 0 0 0 0 0 0 3],		#[0 0 0 0 0 0 0 4],		#[0 0 0 0 0 0 0 5]. "Last OID"	self		assert: command encoding		equals: expectedEncoding
%



set class RsrEncoderTest

method:
verifyControlWord: anIntegerencoding: expected	| actual |	actual := ByteArray streamContents: [:stream | self encoder encodeControlWord: anInteger onto: stream].	self		assert: actual		equals: expected
%



set class RsrEncoderTest

method:
tearDown	registry := connection := nil.	super setUp
%