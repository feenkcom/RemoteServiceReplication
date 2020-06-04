| package |
package := Package name: 'RemoteServiceReplication-Compatibility-Test'.
package paxVersion: 1; basicComment: ''.

package classNames
	add: #RsrClassResolverTestCase;
	add: #RsrMockServer;
	add: #RsrTestingProcessModelTestCase;
	add: #RsrGarbageCollectorTestCase;
	add: #RsrSocketPair;
	add: #RsrSocketTestCase;
	add: #RsrTestCase;
	add: #RsrMockService;
	add: #RsrRegistryTestCase;
	add: #RsrTestingProcessModel;
	add: #RsrMockClient;
	yourself.

package methodNames
	yourself.

package setPrerequisites: #('RemoteServiceReplication').

package!

RsrObject
	subclass: #RsrSocketPair
	instanceVariableNames: 'firstSocket secondSocket'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSocketPair categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

RsrProcessModel
	subclass: #RsrTestingProcessModel
	instanceVariableNames: 'forkedException'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrTestingProcessModel categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

RsrService
	subclass: #RsrMockService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrMockService categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

TestCase
	subclass: #RsrTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrTestCase comment: 'An abstract test class which contains utility methods'!
!RsrTestCase categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

RsrTestCase
	subclass: #RsrClassResolverTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrClassResolverTestCase comment: 'This class contains tests'!
!RsrClassResolverTestCase categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

RsrTestCase
	subclass: #RsrGarbageCollectorTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrGarbageCollectorTestCase comment: 'This class contains tests'!
!RsrGarbageCollectorTestCase categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

RsrMockService
	subclass: #RsrMockClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrMockClient categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

RsrMockService
	subclass: #RsrMockServer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrMockServer categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

RsrTestCase
	subclass: #RsrRegistryTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrRegistryTestCase comment: 'I represent tests for the RsrRegistry.'!
!RsrRegistryTestCase categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

RsrTestCase
	subclass: #RsrSocketTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSocketTestCase comment: 'This class contains tests'!
!RsrSocketTestCase categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

RsrTestCase
	subclass: #RsrTestingProcessModelTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrTestingProcessModelTestCase comment: 'This class contains tests'!
!RsrTestingProcessModelTestCase categoriesForClass!RemoteServiceReplication-Compatibility-Test! !

!RsrMockService class methodsFor!
clientClassName	^#RsrMockClient! !

!RsrMockService class methodsFor!
serverClassName	^#RsrMockServer! !

!RsrSocketPair class methodsFor!
firstSocket: firstSocketsecondSocket: secondSocket	^super new		firstSocket: firstSocket;		secondSocket: secondSocket;		yourself! !

!RsrSocketPair class methodsFor!
new	| listener firstSocket secondSocket |	listener := RsrSocket new.	secondSocket := RsrSocket new.	listener listenOn: self listenPort.	secondSocket		connectToHost: '127.0.0.1'		port: self listenPort.	firstSocket := listener accept.	listener close.	(firstSocket isConnected and: [secondSocket isConnected])		ifFalse: [self error: 'Failed to create socket pair'].	^self		firstSocket: firstSocket		secondSocket: secondSocket! !

!RsrSocketPair class methodsFor!
timeout	^2! !

!RsrSocketPair class methodsFor!
listenPort	^64455! !

!RsrTestCase class methodsFor!
isAbstract	^self == RsrTestCase! !

!RsrSocketTestCase methodsFor!
randomPort	^50123! !

!RsrSocketTestCase methodsFor!
assertWriting: bytesto: writingSocketisReadableOn: readSocket	| readBytes |	writingSocket write: bytes.	readBytes := readSocket read: bytes size.	self		assert: readBytes		equals: bytes! !

!RsrSocketTestCase methodsFor!
testReadAvailable	| pair a b bytes readBytes writingProcess |	pair := RsrSocketPair new.	a := pair firstSocket.	b := pair secondSocket.	bytes := #[1].	self deny: a dataAvailable.	self		assert: a readAvailable		equals: #[].	b write: bytes.	self		assert: a readAvailable		equals: bytes.	self deny: a dataAvailable.	bytes := ByteArray withAll: (1 to: 255).	b write: bytes.	self		assert: a readAvailable		equals: bytes! !

!RsrSocketTestCase methodsFor!
testConnectLocalSockets	| listener server client port |	listener := RsrSocket new.	client := RsrSocket new.	port := self randomPort.	listener listenOn: port.	client		connectToHost: '127.0.0.1'		port: port.	server := listener accept.	listener close.	self		assert: server isConnected;		assert: client isConnected.	self		assertWriting: #(1 2 3 4 5 6 7 8 9 0) asByteArray		to: server		isReadableOn: client.	self		assertWriting: #(0 9 8 7 6 5 4 3 2 1) asByteArray		to: client		isReadableOn: server! !

!RsrSocketTestCase methodsFor!
testHasDataAvailable	| socketPair |	socketPair := RsrSocketPair new.	self deny: socketPair firstSocket dataAvailable.	socketPair secondSocket write: #[1].	self assert: socketPair firstSocket dataAvailable.! !

!RsrSocketTestCase methodsFor!
testConnectToClosedPort	| socket |	socket := RsrSocket new.	self		should: [socket connectToHost: '127.0.0.1' port: 64752]		raise: Error! !

!RsrSocketPair methodsFor!
secondSocket: anObject	secondSocket := anObject! !

!RsrSocketPair methodsFor!
firstStream	^(RsrClassResolver classNamed: #RsrSocketStream) on: firstSocket! !

!RsrSocketPair methodsFor!
close	firstSocket close.	secondSocket close! !

!RsrSocketPair methodsFor!
firstSocket	^ firstSocket! !

!RsrSocketPair methodsFor!
firstSocket: anObject	firstSocket := anObject! !

!RsrSocketPair methodsFor!
secondStream	^(RsrClassResolver classNamed: #RsrSocketStream) on: secondSocket! !

!RsrSocketPair methodsFor!
secondSocket	^ secondSocket! !

!RsrTestingProcessModelTestCase methodsFor!
testCurrentStackDump	| stack |	stack := RsrProcessModel currentStackDump.	self		assert: stack isString;		assert: stack size > 0! !

!RsrTestingProcessModelTestCase methodsFor!
exceptionCase	| sema |	sema := Semaphore new.	RsrProcessModel fork: [[Error signal] ensure: [sema signal]].	sema wait! !

!RsrTestingProcessModelTestCase methodsFor!
testNoException	| testCase |	testCase := self class selector: #noExceptionCase.	self		shouldnt: [testCase runCase]		raise: Exception! !

!RsrTestingProcessModelTestCase methodsFor!
noExceptionCase	| sema |	sema := Semaphore new.	RsrProcessModel fork: [sema signal].	sema wait! !

!RsrTestingProcessModelTestCase methodsFor!
testException	| testCase |	testCase := self class selector: #exceptionCase.	self		should: [testCase runCase]		raise: Exception! !

!RsrTestingProcessModel methodsFor!
forkedException	^forkedException! !

!RsrTestingProcessModel methodsFor!
protect: aBlock	^[aBlock on: Exception do: [:ex | forkedException := ex copy. ex return]]! !

!RsrTestingProcessModel methodsFor!
fork: aBlockat: aPriority	^super		fork: (self protect: aBlock)		at: aPriority! !

!RsrTestingProcessModel methodsFor!
fork: aBlock	^super fork: (self protect: aBlock)! !

!RsrRegistryTestCase methodsFor!
testRemoveKey	| registry client |	registry := RsrRegistry new.	client := RsrMockClient new.	self		assert: (registry removeKey: client _id)		equals: nil.	registry		serviceAt: client _id		put: client.	self		assert: (registry removeKey: client _id) service		identicalTo: client! !

!RsrRegistryTestCase methodsFor!
testIncludesKey	| registry client |	registry := RsrRegistry new.	client := RsrMockClient new.	self deny: (registry includesKey: client _id).	registry		serviceAt: client _id		put: client.	self assert: (registry includesKey: client _id)! !

!RsrRegistryTestCase methodsFor!
testAddServer	| id object registry marker |	marker := Object new.	object := RsrMockServer new.	id := object _id.	registry := RsrRegistry new.	registry		serviceAt: id		put: object.	object := nil.	self maximumReclamation.	object := registry serviceAt: id ifAbsent: [marker].	self		deny: object		equals: marker.	self		assert: object class		equals: RsrMockServer.	self		assert: object _id		equals: id! !

!RsrRegistryTestCase methodsFor!
testAtAtIfAbsent	| registry server id marker |	registry := RsrRegistry new.	server := RsrMockServer new.	id := server _id.	self		should: [registry serviceAt: id]		raise: Error.	marker := Object new.	self		assert: (registry serviceAt: id ifAbsent: [marker])		identicalTo: marker.	registry		serviceAt: id		put: server.	self		assert: (registry serviceAt: id)		identicalTo: server.	self		assert: (registry serviceAt: id ifAbsent: [marker])		identicalTo: server! !

!RsrRegistryTestCase methodsFor!
testAddClient	| id object entry registry marker |	marker := Object new.	object := RsrMockClient new.	id := object _id.	registry := RsrRegistry new.	registry		serviceAt: id		put: object.	self maximumReclamation.	self		assert: (registry serviceAt: id ifAbsent: [marker])		identicalTo: object.	object := nil.	self maximumReclamation.	self		assert: (registry serviceAt: id ifAbsent: [marker])		identicalTo: marker! !

!RsrClassResolverTestCase methodsFor!
testSuccessfulResolution	| actual |	actual := RsrClassResolver classNamed: #Object.	self		assert: actual		identicalTo: Object.	actual := RsrClassResolver		classNamed: #Object		ifAbsent: [self assert: false].	self		assert: actual		identicalTo: Object! !

!RsrClassResolverTestCase methodsFor!
testFailedResolution	| actual marker |	self		should: [RsrClassResolver classNamed: #Xlerb]		raise: RsrUnknownClass.	marker := Object new.	actual := RsrClassResolver		classNamed: #Xlerb		ifAbsent: [marker].	self		assert: actual		identicalTo: marker! !

!RsrClassResolverTestCase methodsFor!
assert: aClassNameresolvesTo: expectedClass	| actualClass |	actualClass := RsrClassResolver classNamed: aClassName.	self		assert: actualClass		identicalTo: expectedClass! !

!RsrMockService methodsFor!
initialize	super initialize.	_id := 1! !

!RsrMockService methodsFor!
isServer	^self class == RsrMockServer! !

!RsrMockService methodsFor!
isClient	^self class == RsrMockClient! !

!RsrMockService methodsFor!
service	^self! !

!RsrTestCase methodsFor!
runCase	| pm |	pm := RsrTestingProcessModel new.	RsrProcessModel current: pm.	[super runCase]		ensure:			[RsrProcessModel resetCurrent].	pm forkedException ifNotNil: [:ex | ex signal]! !

!RsrTestCase methodsFor!
hack: aString	"Placeholder for things that need to be fixed"! !

!RsrTestCase methodsFor!
assert: anObjectidenticalTo: bObject	self assert: anObject == bObject! !

!RsrTestCase methodsFor!
deny: anObjectidenticalTo: bObject	self assert: anObject ~~ bObject! !

!RsrTestCase methodsFor!
fork: aBlock	^RsrProcessModel fork: aBlock! !

!RsrTestCase methodsFor!
assumption: aString	"This method serves as a marker for assumptions made in the tests.	Perhaps some of the senders can be removed in the future."! !

!RsrTestCase methodsFor!
maximumReclamation	self assert: RsrGarbageCollector maximumReclamation! !

!RsrTestCase methodsFor!
defaultTimeLimit	^5 seconds! !

!RsrGarbageCollectorTestCase methodsFor!
testMaximumReclamation	self assert: RsrGarbageCollector maximumReclamation! !