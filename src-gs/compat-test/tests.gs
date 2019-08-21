run
Object
	subclass: #RsrMockService
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrConcurrency
	subclass: #RsrTestingConcurrency
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
TestCase
	subclass: #RsrTestCase
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrGarbageCollectorTestCase
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrRegistryTestCase
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrClassResolverTestCase
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrSocketTestCase
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%





set class RsrMockService

method:
rsrId	^1
%

set class RsrTestingConcurrency

method:
protect: aBlock	^[aBlock on: Exception do: [:ex | "NOP"]]
%



set class RsrTestingConcurrency

method:
fork: aBlockat: aPriority	^super		fork: (self protect: aBlock)		at: aPriority
%



set class RsrTestingConcurrency

method:
fork: aBlock	^super fork: (self protect: aBlock)
%

set class RsrTestCase class

classmethod:
isAbstract	^self == RsrTestCase
%

set class RsrTestCase

method:
hack: aString	"Placeholder for things that need to be fixed"
%



set class RsrTestCase

method:
defaultTimeLimit	^5 seconds
%



set class RsrTestCase

method:
assert: anObjectidenticalTo: bObject	self assert: anObject == bObject
%



set class RsrTestCase

method:
deny: anObjectidenticalTo: bObject	self assert: anObject ~~ bObject
%



set class RsrTestCase

method:
fork: aBlock	^RsrConcurrency fork: aBlock
%



set class RsrTestCase

method:
setUp	super setUp.	RsrConcurrency current: RsrTestingConcurrency new
%



set class RsrTestCase

method:
maximumReclamation	self assert: RsrGarbageCollector maximumReclamation
%



set class RsrTestCase

method:
tearDown	RsrConcurrency resetCurrent.	super tearDown
%

set class RsrGarbageCollectorTestCase

method:
testMaximumReclamation	self assert: RsrGarbageCollector maximumReclamation
%

set class RsrRegistryTestCase

method:
testRegister	| id object registry marker |	marker := Object new.	object := RsrMockService new.	id := object rsrId.	registry := RsrRegistry new.	registry register: object.	self maximumReclamation.	self		assert: (registry at: id ifAbsent: [marker])		identicalTo: object.	object := nil.	self maximumReclamation.	self		assert: (registry at: id ifAbsent: [marker])		identicalTo: marker
%



set class RsrRegistryTestCase

method:
testRetain	| id object registry marker |	marker := Object new.	object := RsrMockService new.	id := object rsrId.	registry := RsrRegistry new.	registry retain: object.	object := nil.	self maximumReclamation.	object := registry at: id ifAbsent: [marker].	self		deny: object		equals: marker.	self		assert: object class		equals: RsrMockService.	self		assert: object rsrId		equals: id
%

set class RsrClassResolverTestCase

method:
assert: aClassNameresolvesTo: expectedClass	| actualClass |	actualClass := RsrClassResolver classNamed: aClassName.	self		assert: actualClass		identicalTo: expectedClass
%



set class RsrClassResolverTestCase

method:
testSuccessfulResolution	| actual |	actual := RsrClassResolver classNamed: #Object.	self		assert: actual		identicalTo: Object.	actual := RsrClassResolver		classNamed: #Object		ifAbsent: [self assert: false].	self		assert: actual		identicalTo: Object
%



set class RsrClassResolverTestCase

method:
testFailedResolution	| actual marker |	self		should: [RsrClassResolver classNamed: #Xlerb]		raise: Error.	marker := Object new.	actual := RsrClassResolver		classNamed: #Xlerb		ifAbsent: [marker].	self		assert: actual		identicalTo: marker
%

set class RsrSocketTestCase

method:
testConnectToClosedPort	| socket |	socket := RsrSocket new.	self		should: [socket connectTo: 64752 on: '127.0.0.1']		raise: Error
%



set class RsrSocketTestCase

method:
assertWriting: bytesto: writingSocketisReadableOn: readSocket	| readBytes |	writingSocket write: bytes.	readBytes := readSocket read: bytes size.	self		assert: readBytes		equals: bytes
%



set class RsrSocketTestCase

method:
randomPort	^50123
%



set class RsrSocketTestCase

method:
testConnectLocalSockets	| listener server client port |	listener := RsrSocket new.	client := RsrSocket new.	port := self randomPort.	listener listenOn: port.	client connectTo: port on: '127.0.0.1'.	server := listener accept.	listener close.	self		assert: server isConnected;		assert: client isConnected.	self		assertWriting: #(1 2 3 4 5 6 7 8 9 0) asByteArray		to: server		isReadableOn: client.	self		assertWriting: #(0 9 8 7 6 5 4 3 2 1) asByteArray		to: client		isReadableOn: server
%