| package |
package := Package name: 'RemoteServiceReplication-Test'.
package paxVersion: 1; basicComment: ''.

package classNames
	add: #RsrReflectedVariableTestServer;
	add: #RsrMockRegistry;
	add: #RsrValueHolder;
	add: #RsrTestService;
	add: #RsrClientTestService;
	add: #RsrSpeciesEquality;
	add: #RsrRemoteActionClient;
	add: #RsrMockEncoder;
	add: #RsrReflectedVariableTestServiceB;
	add: #RsrServerNoInstVars;
	add: #RsrServiceTest;
	add: #RsrRemoteActionServer;
	add: #RsrThreadSafeNumericSpigotTest;
	add: #RsrStressTest;
	add: #RsrSystemTestCase;
	add: #RsrRemoteAction;
	add: #RsrCodecTest;
	add: #RsrValueHolderClient;
	add: #RsrServiceNoInstVars;
	add: #RsrPromiseTest;
	add: #RsrEncoderTest;
	add: #RsrConcurrentTestService;
	add: #RsrValueHolderServer;
	add: #RsrSignalErrorInAsString;
	add: #RsrClientReferenceService;
	add: #RsrSameTemplateAndClientService;
	add: #RsrConnectionTestCase;
	add: #RsrDecoderTest;
	add: #RsrConcurrentTestServer;
	add: #RsrRetainAnalysisTest;
	add: #RsrForwarderTest;
	add: #RsrConcurrentTestClient;
	add: #RsrReflectedVariableTestClient;
	add: #RsrServiceReferenceService;
	add: #RsrServerTestService;
	add: #RsrMockConnection;
	add: #RsrServerReferenceService;
	add: #RsrDifferentServerService;
	add: #RsrLifetimeTest;
	add: #RsrReflectedVariableTestServiceA;
	add: #RsrClientNoInstVars;
	add: #RsrSocketStreamTestCase;
	add: #RsrNumericSpigotTest;
	add: #RsrMessageSendingTest;
	yourself.

package methodNames
	yourself.

package setPrerequisites: #('RemoteServiceReplication-Platform-Test').

package!

RsrObject
	subclass: #RsrMockConnection
	instanceVariableNames: 'forwarderClass lastMessage registry idSpigot'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrMockConnection categoriesForClass!RemoteServiceReplication-Test! !

RsrObject
	subclass: #RsrMockRegistry
	instanceVariableNames: 'objects idSpigot'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrMockRegistry categoriesForClass!RemoteServiceReplication-Test! !

RsrObject
	subclass: #RsrSignalErrorInAsString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSignalErrorInAsString categoriesForClass!RemoteServiceReplication-Test! !

RsrService
	subclass: #RsrConcurrentTestService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrConcurrentTestService categoriesForClass!RemoteServiceReplication-Test! !

RsrService
	subclass: #RsrReflectedVariableTestServiceA
	instanceVariableNames: 'varA'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrReflectedVariableTestServiceA categoriesForClass!RemoteServiceReplication-Test! !

RsrService
	subclass: #RsrRemoteAction
	instanceVariableNames: 'sharedVariable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRemoteAction categoriesForClass!RemoteServiceReplication-Test! !

RsrService
	subclass: #RsrSameTemplateAndClientService
	instanceVariableNames: 'replicated1 replicated2'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSameTemplateAndClientService categoriesForClass!RemoteServiceReplication-Test! !

RsrService
	subclass: #RsrServiceNoInstVars
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceNoInstVars categoriesForClass!RemoteServiceReplication-Test! !

RsrService
	subclass: #RsrServiceReferenceService
	instanceVariableNames: 'service'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceReferenceService categoriesForClass!RemoteServiceReplication-Test! !

RsrService
	subclass: #RsrTestService
	instanceVariableNames: 'sharedVariable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrTestService categoriesForClass!RemoteServiceReplication-Test! !

RsrService
	subclass: #RsrValueHolder
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrValueHolder categoriesForClass!RemoteServiceReplication-Test! !

RsrServiceNoInstVars
	subclass: #RsrClientNoInstVars
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClientNoInstVars categoriesForClass!RemoteServiceReplication-Test! !

RsrServiceReferenceService
	subclass: #RsrClientReferenceService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClientReferenceService categoriesForClass!RemoteServiceReplication-Test! !

RsrTestService
	subclass: #RsrClientTestService
	instanceVariableNames: 'privateVariable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClientTestService categoriesForClass!RemoteServiceReplication-Test! !

RsrTestCase
	subclass: #RsrCodecTest
	instanceVariableNames: 'registry decoder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrCodecTest comment: 'This class contains tests'!
!RsrCodecTest categoriesForClass!RemoteServiceReplication-Test! !

RsrConcurrentTestService
	subclass: #RsrConcurrentTestClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrConcurrentTestClient categoriesForClass!RemoteServiceReplication-Test! !

RsrConcurrentTestService
	subclass: #RsrConcurrentTestServer
	instanceVariableNames: 'counter semaphore stashedProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrConcurrentTestServer categoriesForClass!RemoteServiceReplication-Test! !

RsrSameTemplateAndClientService
	subclass: #RsrDifferentServerService
	instanceVariableNames: 'private1'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDifferentServerService categoriesForClass!RemoteServiceReplication-Test! !

RsrTestCase
	subclass: #RsrForwarderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrForwarderTest comment: 'This class contains tests'!
!RsrForwarderTest categoriesForClass!RemoteServiceReplication-Test! !

RsrEncoder
	subclass: #RsrMockEncoder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrMockEncoder categoriesForClass!RemoteServiceReplication-Test! !

RsrTestCase
	subclass: #RsrNumericSpigotTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrNumericSpigotTest comment: 'This class contains tests'!
!RsrNumericSpigotTest categoriesForClass!RemoteServiceReplication-Test! !

RsrTestCase
	subclass: #RsrPromiseTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrPromiseTest comment: 'This class contains tests'!
!RsrPromiseTest categoriesForClass!RemoteServiceReplication-Test! !

RsrReflectedVariableTestServiceA
	subclass: #RsrReflectedVariableTestServiceB
	instanceVariableNames: 'varB'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrReflectedVariableTestServiceB categoriesForClass!RemoteServiceReplication-Test! !

RsrRemoteAction
	subclass: #RsrRemoteActionClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRemoteActionClient categoriesForClass!RemoteServiceReplication-Test! !

RsrRemoteAction
	subclass: #RsrRemoteActionServer
	instanceVariableNames: 'action'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRemoteActionServer categoriesForClass!RemoteServiceReplication-Test! !

RsrTestCase
	subclass: #RsrRetainAnalysisTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrRetainAnalysisTest comment: 'This class contains tests'!
!RsrRetainAnalysisTest categoriesForClass!RemoteServiceReplication-Test! !

RsrServiceNoInstVars
	subclass: #RsrServerNoInstVars
	instanceVariableNames: 'marker'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServerNoInstVars categoriesForClass!RemoteServiceReplication-Test! !

RsrServiceReferenceService
	subclass: #RsrServerReferenceService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServerReferenceService categoriesForClass!RemoteServiceReplication-Test! !

RsrTestService
	subclass: #RsrServerTestService
	instanceVariableNames: 'privateVariable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServerTestService categoriesForClass!RemoteServiceReplication-Test! !

RsrTestCase
	subclass: #RsrSocketStreamTestCase
	instanceVariableNames: 'serverStream clientStream'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSocketStreamTestCase comment: 'This class contains tests'!
!RsrSocketStreamTestCase categoriesForClass!RemoteServiceReplication-Test! !

RsrTestCase
	subclass: #RsrSystemTestCase
	instanceVariableNames: 'connectionA connectionB'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSystemTestCase comment: 'This class contains tests'!
!RsrSystemTestCase categoriesForClass!RemoteServiceReplication-Test! !

RsrValueHolder
	subclass: #RsrValueHolderClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrValueHolderClient categoriesForClass!RemoteServiceReplication-Test! !

RsrValueHolder
	subclass: #RsrValueHolderServer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrValueHolderServer categoriesForClass!RemoteServiceReplication-Test! !

RsrSystemTestCase
	subclass: #RsrConnectionTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrConnectionTestCase comment: 'This class contains tests'!
!RsrConnectionTestCase categoriesForClass!RemoteServiceReplication-Test! !

RsrCodecTest
	subclass: #RsrDecoderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrDecoderTest comment: 'This class contains tests'!
!RsrDecoderTest categoriesForClass!RemoteServiceReplication-Test! !

RsrCodecTest
	subclass: #RsrEncoderTest
	instanceVariableNames: 'connection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrEncoderTest comment: 'This class contains tests'!
!RsrEncoderTest categoriesForClass!RemoteServiceReplication-Test! !

RsrSystemTestCase
	subclass: #RsrLifetimeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrLifetimeTest comment: 'This class contains tests'!
!RsrLifetimeTest categoriesForClass!RemoteServiceReplication-Test! !

RsrSystemTestCase
	subclass: #RsrMessageSendingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrMessageSendingTest comment: 'This class contains tests'!
!RsrMessageSendingTest categoriesForClass!RemoteServiceReplication-Test! !

RsrReflectedVariableTestServiceB
	subclass: #RsrReflectedVariableTestClient
	instanceVariableNames: 'private'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrReflectedVariableTestClient categoriesForClass!RemoteServiceReplication-Test! !

RsrReflectedVariableTestServiceB
	subclass: #RsrReflectedVariableTestServer
	instanceVariableNames: 'private'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrReflectedVariableTestServer categoriesForClass!RemoteServiceReplication-Test! !

RsrSystemTestCase
	subclass: #RsrServiceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrServiceTest comment: 'This class contains tests'!
!RsrServiceTest categoriesForClass!RemoteServiceReplication-Test! !

RsrSystemTestCase
	subclass: #RsrSpeciesEquality
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSpeciesEquality comment: 'This class contains tests'!
!RsrSpeciesEquality categoriesForClass!RemoteServiceReplication-Test! !

RsrSystemTestCase
	subclass: #RsrStressTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrStressTest comment: 'This class contains tests'!
!RsrStressTest categoriesForClass!RemoteServiceReplication-Test! !

RsrNumericSpigotTest
	subclass: #RsrThreadSafeNumericSpigotTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrThreadSafeNumericSpigotTest comment: 'This class contains tests'!
!RsrThreadSafeNumericSpigotTest categoriesForClass!RemoteServiceReplication-Test! !

!RsrConcurrentTestService class methodsFor!
clientClassName	^#RsrConcurrentTestClient! !

!RsrConcurrentTestService class methodsFor!
templateClassName	^#RsrConcurrentTestService! !

!RsrConcurrentTestService class methodsFor!
serverClassName	^#RsrConcurrentTestServer! !

!RsrCodecTest class methodsFor!
isAbstract	^self == RsrCodecTest! !

!RsrConcurrentTestServer class methodsFor!
initialCounter	^0! !

!RsrValueHolder class methodsFor!
clientClassName	^#RsrValueHolderClient! !

!RsrValueHolder class methodsFor!
templateClassName	^#RsrValueHolder! !

!RsrValueHolder class methodsFor!
value: anRsrObject	^self new		value: anRsrObject;		yourself! !

!RsrValueHolder class methodsFor!
serverClassName	^#RsrValueHolderServer! !

!RsrServiceReferenceService class methodsFor!
clientClassName	^#RsrClientReferenceService! !

!RsrServiceReferenceService class methodsFor!
service: aService	^self new		service: aService;		yourself! !

!RsrServiceReferenceService class methodsFor!
templateClassName	^#RsrServiceReferenceService! !

!RsrServiceReferenceService class methodsFor!
serverClassName	^#RsrServerReferenceService! !

!RsrSameTemplateAndClientService class methodsFor!
clientClassName	^self templateClassName! !

!RsrSameTemplateAndClientService class methodsFor!
templateClassName	^#RsrSameTemplateAndClientService! !

!RsrSameTemplateAndClientService class methodsFor!
serverClassName	^#RsrDifferentServerService! !

!RsrSystemTestCase class methodsFor!
isAbstract	^self == RsrSystemTestCase! !

!RsrTestService class methodsFor!
clientClassName	^#RsrClientTestService! !

!RsrTestService class methodsFor!
templateClassName	^#RsrTestService! !

!RsrTestService class methodsFor!
serverClassName	^#RsrServerTestService! !

!RsrRemoteAction class methodsFor!
templateClassName	^#RsrRemoteAction! !

!RsrRemoteAction class methodsFor!
sharedVariable: anObject	^self new		sharedVariable: anObject;		yourself! !

!RsrMockConnection class methodsFor!
forwarderClass: aClass 	^self new		forwarderClass: aClass;		yourself! !

!RsrReflectedVariableTestServiceB class methodsFor!
clientClassName	^#RsrReflectedVariableTestClient! !

!RsrReflectedVariableTestServiceB class methodsFor!
templateClassName	^#RsrReflectedVariableTestServiceB! !

!RsrReflectedVariableTestServiceB class methodsFor!
serverClassName	^#RsrReflectedVariableTestServer! !

!RsrServiceNoInstVars class methodsFor!
clientClassName	^#RsrClientNoInstVars! !

!RsrServiceNoInstVars class methodsFor!
templateClassName	^#RsrServiceNoInstVars! !

!RsrServiceNoInstVars class methodsFor!
serverClassName	^#RsrServerNoInstVars! !

!RsrForwarderTest methodsFor!
testForwarding	| rsrObject id remoteInterface forwarder message |	rsrObject := RsrTestService clientClass new.	id := 1.	remoteInterface := RsrMockConnection forwarderClass: RsrForwarder.	rsrObject		_id: id		connection: remoteInterface.	forwarder := rsrObject remoteSelf.	forwarder		arg1: 15		arg2: 42.	message := remoteInterface lastMessage.	self		assert: message transaction		equals: 1.	self		assert: message receiver		equals: rsrObject.	self		assert: message selector		equals: #arg1:arg2:.	self		assert: message arguments		equals: #(15 42).! !

!RsrStressTest methodsFor!
testRepeatedSendReceive1KBytes	self repeatedlySend: (ByteArray new: 1024)! !

!RsrStressTest methodsFor!
testRepeatedSendReceive2KBytes	self repeatedlySend: (ByteArray new: 1024 *2)! !

!RsrStressTest methodsFor!
repeatedlySend: anObject	| client server |	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [:object | object].	self repetitions timesRepeat: [client value: anObject].	self assert: true. "If we get to this point, the sends have all successed"! !

!RsrStressTest methodsFor!
testRepeatedUnarySends	| client server |	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [server].	self repetitions timesRepeat: [client value].	self assert: true. "If we get to this point, the sends have all successed"! !

!RsrStressTest methodsFor!
repetitions	^1000! !

!RsrStressTest methodsFor!
testRepeatedSendReceive1MBytes	self repeatedlySend: (ByteArray new: 1024 squared)! !

!RsrRetainAnalysisTest methodsFor!
testServiceAllDataObjects	"While this code is structurally similar to #testClientNoInstVars, it ensures	that Data Objects are actually encoded in-line."	| client registry analysis expected |	client := RsrRemoteAction clientClass new.	registry := RsrMockRegistry new.	analysis := self analyze: client.	expected := OrderedCollection		with: (RsrRetainObject object: client encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self assert: client isMirrored! !

!RsrRetainAnalysisTest methodsFor!
assertCycle: anObject	self		should: [self analyze: anObject]		raise: RsrCycleDetected! !

!RsrRetainAnalysisTest methodsFor!
testSetCycle	| set |	set := Set new.	set add: set.	self assertCycle: set.	set := Set new.	set add: (Array with: set).	self assertCycle: set! !

!RsrRetainAnalysisTest methodsFor!
analyze: anObject	| analysis |	analysis := RsrRetainAnalysis		roots: (Array with: anObject)		connection: RsrMockConnection new.	analysis perform.	^analysis! !

!RsrRetainAnalysisTest methodsFor!
testArrayCycle	| array |	array := Array new: 1.	array		at: 1		put: array.	self assertCycle: array.	array		at: 1		put: { array }.	self assertCycle: array! !

!RsrRetainAnalysisTest methodsFor!
testNewServiceInArray	"Ensure a new service in a collection is properly tagged"	| service analysis expected |	service := RsrServerNoInstVars new.	analysis := self analyze: (Array with: service).	expected := OrderedCollection		with: (RsrRetainObject object: service encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self assert: service isMirrored! !

!RsrRetainAnalysisTest methodsFor!
testServiceNoInstVars	| client analysis expected |	client := RsrClientNoInstVars new.	analysis := self analyze: client.	expected := OrderedCollection		with: (RsrRetainObject object: client encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self assert: client isMirrored! !

!RsrRetainAnalysisTest methodsFor!
testOrderedCollectionCycle	| oc |	oc := OrderedCollection new.	oc add: oc.	self assertCycle: oc.	oc := OrderedCollection with: (Array with: oc).	self assertCycle: oc.! !

!RsrRetainAnalysisTest methodsFor!
testServiceWithCycle	"Cycles are disallowed for our POC. Perhaps they will get added later?"	| rootClient referencedClient |	rootClient := RsrRemoteAction new.	referencedClient := RsrRemoteAction sharedVariable: rootClient.	rootClient sharedVariable: referencedClient.	self assertCycle: rootClient! !

!RsrRetainAnalysisTest methodsFor!
testDictionaryCycle	| dictionary |	dictionary := Dictionary new.	dictionary		at: 1		put: dictionary.	self assertCycle: dictionary.	dictionary removeKey: 1.	dictionary		at: dictionary		put: 1.	self assertCycle: dictionary! !

!RsrRetainAnalysisTest methodsFor!
testNewServicesInDictionary	"Ensure a new service in a collection is properly tagged"	| key value dictionary analysis expected |	key := RsrServerNoInstVars new.	value := RsrServerNoInstVars new.	dictionary := Dictionary new		at: key put: value;		yourself.	analysis := self analyze: dictionary.	expected := OrderedCollection		with: (RsrRetainObject object: key encoding: ByteArray new)		with: (RsrRetainObject object: value encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self		assert: key isMirrored;		assert: value isMirrored! !

!RsrRetainAnalysisTest methodsFor!
testServiceReferencingAnotherService	"While this code is structurally similar to #testClientNoInstVars, it ensures	that Data Objects are actually encoded in-line."	| referencedService client analysis expected |	referencedService := RsrRemoteAction clientClass new.	client := RsrRemoteAction clientClass sharedVariable: referencedService.	analysis := self analyze: client.	expected := OrderedCollection		with: (RsrRetainObject object: referencedService encoding: ByteArray new)		with: (RsrRetainObject object: client encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self		assert: client isMirrored;		assert: referencedService isMirrored! !

!RsrConcurrentTestServer methodsFor!
counter: anArray	counter := anArray! !

!RsrConcurrentTestServer methodsFor!
stashedProcess	^stashedProcess! !

!RsrConcurrentTestServer methodsFor!
delayedCounter	semaphore signal.	(Delay forSeconds: 2) wait.	^counter at: 1! !

!RsrConcurrentTestServer methodsFor!
counterWithIncrement	^[counter at: 1] ensure: [counter at: 1 put: (counter at: 1) + 1]! !

!RsrConcurrentTestServer methodsFor!
stashProcess	stashedProcess := Processor activeProcess! !

!RsrConcurrentTestServer methodsFor!
semaphore: aSemaphore	semaphore := aSemaphore! !

!RsrPromiseTest methodsFor!
testFulfillment	| promise expected semaphore |	promise := RsrPromise new.	expected := Object new.	self fork:		[(Delay forSeconds: 1) wait.		promise fulfill: expected].	self		assert: promise value		identicalTo: expected.	promise := RsrPromise new.	semaphore := Semaphore new.	[promise fulfill: expected.	semaphore signal] fork.	semaphore wait.	self		assert: promise value		identicalTo: expected! !

!RsrPromiseTest methodsFor!
testError	| promise semaphore |	promise := RsrPromise new.	self fork:		[(Delay forSeconds: 1) wait.		promise error: Error new].	self		should: [promise value]		raise: Error.	promise := RsrPromise new.	semaphore := Semaphore new.	self fork:		[promise error: Error new.		semaphore signal].	self		should: [promise value]		raise: Error! !

!RsrEncoderTest methodsFor!
testReleaseObjects	| command expectedEncoding |	command := RsrReleaseObjects oids: #(1 2 3 4 5).	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 3], "ReleaseObjects Command"		#[0 0 0 0 0 0 0 5], "Num OIDS"		#[0 0 0 0 0 0 0 1], "First OID"		#[0 0 0 0 0 0 0 2],		#[0 0 0 0 0 0 0 3],		#[0 0 0 0 0 0 0 4],		#[0 0 0 0 0 0 0 5]. "Last OID"	self		assert: command encoding		equals: expectedEncoding! !

!RsrEncoderTest methodsFor!
testDeliverErrorResponse	| error remoteError command expectedEncoding |	error := Error new		tag: 'tag';		messageText: 'message text';		yourself.	remoteError := RsrRemoteError from: error.	remoteError stack: 'stack dump'.	command := RsrDeliverErrorResponse		transaction: 1		remoteError: remoteError.	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 4], "DeliverErrorResponse Command"		#[0 0 0 0 0 0 0 1], "Transaction Id"		#[0 0 0 0 0 0 0 0], "originalClassName: Symbol encoding"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 5], "Length of UTF-8 bytes"		#[69 114 114 111 114], "#Error"		#[0 0 0 0 0 0 0 0], "OID = 0"		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"		#[0 0 0 0 0 0 0 3], "length"		#[116 97 103],	 "tag"		#[0 0 0 0 0 0 0 0], "OID = 0"		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"		#[0 0 0 0 0 0 0 12], "length"		#[109 101 115 115 97 103 101 32 116 101 120 116],	 "message text"		#[0 0 0 0 0 0 0 0], "OID = 0"		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"		#[0 0 0 0 0 0 0 10], "length"		#[115 116 97 99 107 32 100 117 109 112].	 "stack dump"	self		assert: command encoding		equals: expectedEncoding! !

!RsrEncoderTest methodsFor!
tearDown	registry := connection := nil.	super setUp! !

!RsrEncoderTest methodsFor!
testUnsupportedObject	self		should: [self encoder encodeObject: Object new]		raise: RsrUnsupportedObject.	self		should: [self encoder encodeReferenceOf: Object new onto: (WriteStream on: ByteArray new)]		raise: RsrUnsupportedObject! !

!RsrEncoderTest methodsFor!
testRetainObject	| service command expectedEncoding |	service := RsrClientNoInstVars new.	self register: service.	command := RsrRetainObject object: service.	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 0], "RetainObject Command"		#[0 0 0 0 0 0 0 0], "ServiceType Object"		#[0 0 0 0 0 0 0 1], "Service OID"		#[0 0 0 0 0 0 0 0], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 19], "Length of UTF-8 bytes"		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"	self		assert: command encoding		equals: expectedEncoding! !

!RsrEncoderTest methodsFor!
testServiceReferenceService	| rootService referencedService encodedObject expectedEncoding |	referencedService := RsrClientNoInstVars new.	rootService := RsrClientReferenceService service: referencedService.	self		register: rootService;		register: referencedService.	encodedObject := self encoder encodeObject: rootService.	expectedEncoding :=		#[0 0 0 0 0 0 0 0], "type"		#[0 0 0 0 0 0 0 1], "rootService's OID = 1"		#[0 0 0 0 0 0 0 1], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"		#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"		#[0 0 0 0 0 0 0 25], "Length of UTF-8 encoded bytes"		#[82 115 114 83 101 114 118 101 114 82 101 102 101 114 101 110 99 101 83 101 114 118 105 99 101],		#[0 0 0 0 0 0 0 2]. "#RsrServerReferenceService"	self		assert: encodedObject		equals: expectedEncoding.	encodedObject := self encoder encodeObject: referencedService.	expectedEncoding :=		#[0 0 0 0 0 0 0 0], "type"		#[0 0 0 0 0 0 0 2], "referencedService's OID = 2"		#[0 0 0 0 0 0 0 0], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"		#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"		#[0 0 0 0 0 0 0 19], "Length of UTF-8 encoded bytes"		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"	self		assert: encodedObject		equals: expectedEncoding! !

!RsrEncoderTest methodsFor!
testDeliverResponse	| response command expectedEncoding |	response := #responseSymbol.	command := RsrDeliverResponse		transaction: 1		response: response		roots: (Array with: response).	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"		#[0 0 0 0 0 0 0 1], "Transaction Id"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 14], "Length of UTF-8 bytes"		#[114 101 115 112 111 110 115 101 83 121 109 98 111 108]. "#responseSymbol"	self		assert: command encoding		equals: expectedEncoding! !

!RsrEncoderTest methodsFor!
verifyImmediate: anObjectencoding: expected	| actual |	actual := ByteArray streamContents: [:stream | self encoder encodeReferenceOf: anObject onto: stream].	self		assert: actual		equals: expected! !

!RsrEncoderTest methodsFor!
testServiceNoInstVars	| rootService encodedBytes expectedEncoding |	rootService := RsrClientNoInstVars new.	self register: rootService.	encodedBytes := self encoder encodeObject: rootService.	expectedEncoding := self serviceNoInstVarsEncoding.	self		assert: encodedBytes		equals: expectedEncoding! !

!RsrEncoderTest methodsFor!
verifyControlWord: anIntegerencoding: expected	| actual |	actual := ByteArray streamContents: [:stream | self encoder encodeControlWord: anInteger onto: stream].	self		assert: actual		equals: expected! !

!RsrEncoderTest methodsFor!
register: aService	aService		_id: connection oidSpigot next		connection: connection.	registry		at: aService _id		put: aService! !

!RsrEncoderTest methodsFor!
testSendMessage	| service command expectedEncoding |	service := RsrClientNoInstVars new.	self register: service.	command := RsrSendMessage		transaction: 1		receiver: service		selector: #return42		arguments: #().	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 1], "SendMessage Command"		#[0 0 0 0 0 0 0 1], "Transaction ID"		#[0 0 0 0 0 0 0 0], "Argument Count"		#[0 0 0 0 0 0 0 1], "Receiver OID"		#[0 0 0 0 0 0 0 0], "Selector Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 8], "Length of UTF-8 bytes"		#[114 101 116 117 114 110 52 50]. "#return42"	self		assert: command encoding		equals: expectedEncoding! !

!RsrEncoderTest methodsFor!
setUp	super setUp.	registry := RsrMockRegistry new.	connection := RsrMockConnection new! !

!RsrRemoteAction methodsFor!
sharedVariable: anObject	sharedVariable := anObject! !

!RsrRemoteAction methodsFor!
sharedVariable	^sharedVariable! !

!RsrSystemTestCase methodsFor!
serviceFactoryB	^connectionB serviceFactory! !

!RsrSystemTestCase methodsFor!
tearDown	connectionA ifNotNil: [:conn | conn close].	connectionB ifNotNil: [:conn | conn close].	connectionA := connectionB := nil.	super tearDown! !

!RsrSystemTestCase methodsFor!
setUp	| port semaphore |	super setUp.	port := 64455.	semaphore := Semaphore new.	self		fork: [[connectionA := RsrConnection acceptOn: port] ensure: [semaphore signal]];		fork: [[connectionB := RsrConnection connectToHost: '127.0.0.1' port: port] ensure: [semaphore signal]].	semaphore wait; wait.	self		assert: connectionA isOpen;		assert: connectionB isOpen! !

!RsrSystemTestCase methodsFor!
serviceFactoryA	^connectionA serviceFactory! !

!RsrServiceReferenceService methodsFor!
service: anObject	service := anObject! !

!RsrServiceReferenceService methodsFor!
service	^ service! !

!RsrConcurrentTestClient methodsFor!
delayedCounter	^remoteSelf delayedCounter! !

!RsrConcurrentTestClient methodsFor!
counterWithIncrement	^remoteSelf counterWithIncrement! !

!RsrConcurrentTestClient methodsFor!
stashProcess	remoteSelf stashProcess! !

!RsrDifferentServerService methodsFor!
private1	^private1! !

!RsrDifferentServerService methodsFor!
private1: anObject	private1 := anObject! !

!RsrSignalErrorInAsString methodsFor!
asString	^Error signal! !

!RsrServiceNoInstVars methodsFor!
sendReturnArgument: anObject	^remoteSelf returnArgument: anObject! !

!RsrServiceNoInstVars methodsFor!
returnArgument: anObject	^anObject! !

!RsrThreadSafeNumericSpigotTest methodsFor!
spigotClass	^RsrThreadSafeNumericSpigot! !

!RsrMockRegistry methodsFor!
objects	^objects! !

!RsrMockRegistry methodsFor!
connection	^RsrMockConnection new! !

!RsrMockRegistry methodsFor!
initialize	super initialize.	objects := OrderedCollection new! !

!RsrMockRegistry methodsFor!
objects: anObject	objects := anObject! !

!RsrMockRegistry methodsFor!
register: anRsrObject	self objects add: anRsrObject! !

!RsrMockRegistry methodsFor!
newId	^idSpigot next! !

!RsrMockRegistry methodsFor!
retain: anRsrObject	self objects add: anRsrObject! !

!RsrMockRegistry methodsFor!
at: aKeyput: aService	self objects add: aService! !

!RsrMockRegistry methodsFor!
remember: anRsrObject	self objects add: anRsrObject! !

!RsrSpeciesEquality methodsFor!
testUnicodeString	self verify: self unicodeString! !

!RsrSpeciesEquality methodsFor!
testServiceWithUnsupportedObject	| service |	service := connectionA serviceFor: #RsrClientNoInstVars.	self		should: [service sendReturnArgument: (RsrValueHolderClient value: Object new)]		raise: RsrUnsupportedObject! !

!RsrSpeciesEquality methodsFor!
testCharacter	self		verify: (Character codePoint: 0);		verify: (Character codePoint: 65);		verify: $A;		verify: (Character codePoint: 16r01D4);		verify: (Character codePoint: 16r8334)! !

!RsrSpeciesEquality methodsFor!
testArray	self		verify: #();		verify: (Array withAll: self basicExamples)! !

!RsrSpeciesEquality methodsFor!
basicExamples	"Give a samples of each species to ensure Collection classes are able to encode each type successfully."	^{RsrClientNoInstVars new.	#h.	#''.	'h'.	''.	0.	234.	-97.	$s.	nil.	true.	false.	{}.	{RsrClientNoInstVars new. {}.}.	#[].	#[123].	Set new.	Set with: 42.	OrderedCollection new.	OrderedCollection with: #x.	Dictionary new.	Dictionary new at: #key put: #value; yourself.	RsrDateAndTimeSpecies posixEpoch.	RsrDateAndTimeSpecies fromMicroseconds: -1000000. "1969-12-31T23:59:59-00:00"}! !

!RsrSpeciesEquality methodsFor!
verify: anObject	"Send <anObject> through RSR and have it returned. Assert it is equivalent."	| client server |	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [:object | server sharedVariable: object. object].	self		assert: (client value: anObject)		equals: anObject.	self		assert: client sharedVariable		equals: anObject! !

!RsrSpeciesEquality methodsFor!
testString	self		verify: '';		verify: 'string'! !

!RsrSpeciesEquality methodsFor!
unicodeString	^String		with: $a		with: (Character codePoint: 16r8349)		with: (Character codePoint: 16r10E60)! !

!RsrSpeciesEquality methodsFor!
testByteArray	self		verify: #[];		verify: (ByteArray withAll: (0 to: 255));		verify: (ByteArray new: 1024)! !

!RsrSpeciesEquality methodsFor!
testUnicodeSymbol	self verify: self unicodeString asSymbol! !

!RsrSpeciesEquality methodsFor!
testBoolean	self		verify: true;		verify: false! !

!RsrSpeciesEquality methodsFor!
testUndefinedObject	self verify: nil! !

!RsrSpeciesEquality methodsFor!
testSet	self		verify: Set new;		verify: (Set withAll: self basicExamples)! !

!RsrSpeciesEquality methodsFor!
testDictionary	| example |	example := Dictionary new.	self verify: example.	self basicExamples do: [:each | each ifNotNil: [example at: each put: each]].	example at: #testDictionaryPrivateKey put: nil.	self verify: example! !

!RsrSpeciesEquality methodsFor!
testDateAndTime	self		verify: (RsrDateAndTimeSpecies fromMicroseconds: -491277642567488); "1954-06-07T14:59:17.432512-07:00"		verify: (RsrDateAndTimeSpecies fromMicroseconds: 1562692562657612). "2019-07-09T10:16:02.657612-07:00"! !

!RsrSpeciesEquality methodsFor!
testService	| clientClass serverClass |	clientClass := RsrRemoteAction clientClass.	serverClass := RsrRemoteAction serverClass.	self		verify: clientClass new;		verify: (clientClass sharedVariable: clientClass new);		verify: (serverClass sharedVariable: clientClass new)! !

!RsrSpeciesEquality methodsFor!
testOrderedCollection	self		verify: OrderedCollection new;		verify: (OrderedCollection withAll: self basicExamples)! !

!RsrSpeciesEquality methodsFor!
testInteger	self		verify: 0;		verify: -1;		verify: 1;		verify: (2 raisedTo: 32);		verify: (2 raisedTo: 32) negated;		verify: (2 raisedTo: 64);		verify: (2 raisedTo: 64) negated;		verify: 4598754392654025898794;		verify: -13750198234577893465! !

!RsrSpeciesEquality methodsFor!
testSymbol	self		verify: #'';		verify: #symbol! !

!RsrConnectionTestCase methodsFor!
testWaitUntilClose	| semaphore marker |	semaphore := Semaphore new.	marker := false.	self		fork:			[semaphore signal.			[connectionB waitUntilClose.			marker := true]				ensure: [semaphore signal]].	semaphore wait.	self deny: marker.	connectionA close.	semaphore wait.	self assert: marker! !

!RsrSameTemplateAndClientService methodsFor!
replicated2: anObject	replicated2 := anObject! !

!RsrSameTemplateAndClientService methodsFor!
replicated1	^replicated1! !

!RsrSameTemplateAndClientService methodsFor!
replicated1: anObject	replicated1 := anObject! !

!RsrSameTemplateAndClientService methodsFor!
replicated2	^replicated2! !

!RsrMessageSendingTest methodsFor!
testChangeRemoteState	| marker client server |	marker := false.	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [marker := true].	client value.	self assert: marker! !

!RsrMessageSendingTest methodsFor!
testReturnNewServiceInArray	| client server array returnedService |	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [Array with: RsrValueHolderServer new].	array := client value.	returnedService := array first.	self		assert: returnedService class		equals: RsrValueHolderClient! !

!RsrMessageSendingTest methodsFor!
testReturnInvalidObject	| client server exception |				client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [Object new].	self		should: [client value]		raise: RsrRemoteError.	[client value]		on: RsrRemoteError		do: [:ex | exception := ex. ex return].	self		assert: exception class		equals: RsrRemoteError.	self		assert: exception originalClassName		equals: #RsrUnsupportedObject.	self		assert: exception tag		equals: 'Instances of Object cannot be serialized'.	self		assert: exception messageText		equals: 'Instances of Object cannot be serialized'.	self		assert: exception stack isString;		assert: exception stack size > 0! !

!RsrMessageSendingTest methodsFor!
testReturnSymbol	| client server symbol result |	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	symbol := #testSymbol.	server action: [symbol].	result := client value.	self		assert: result		equals: symbol! !

!RsrMessageSendingTest methodsFor!
testReturnArgument	| client server arguments dt response |	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [:object | object].	arguments := OrderedCollection new		addAll: #( '' #symbol 'string' $h 0 -14 14 18446744073709551616 -18446744073709551616 nil true false ); 		add: (Character codePoint: 16r259F);		add: (Dictionary new at: 1 put: 2; yourself);		add: (Set with: 14);		add: #[1 2 3 4];		add: (OrderedCollection with: 42 with: 43);		add: #(1 2 #(nil));		yourself.	dt := RsrDateAndTimeSpecies now.	response := client value: dt.	self		assert: (dt asSeconds * 1000000) rounded		equals: (response asSeconds * 1000000) rounded.	arguments		do:			[:each | | result |			result := client value: each.			self				assert: result				equals: each].	arguments		do:			[:each | | result |			result := server value: each.			self				assert: result				equals: each].	self		assert: (client value: arguments)		equals: arguments.	self		assert: (server value: arguments)		equals: arguments.	self		assert: (client value: client)		identicalTo: client! !

!RsrMessageSendingTest methodsFor!
testReturnNewService	| client server returnedService |	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [RsrValueHolderServer new].	returnedService := client value.	self		assert: returnedService class		equals: RsrValueHolderClient! !

!RsrMessageSendingTest methodsFor!
testSendInvalidObject	| client server |				client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [:arg | arg].	self		should: [client value: Object new]		raise: RsrUnsupportedObject! !

!RsrMessageSendingTest methodsFor!
testReturnAlsoUpdatesLocalService	"Ensure that when the remote peer service returns a value,	that it is also sent to update the local service."	| client server value response |	client := self serviceFactoryA serviceFor: #RsrReflectedVariableTestServiceB.	server := connectionB registry serviceAt: client _id.	value := 42.	self		deny: client varA		equals: value.	self		deny: client varB		equals: value.	response := client setVarsToAndReturn: value.	self		assert: response		equals: value.	self		assert: server varA		equals: value.	self		assert: server varB		equals: value.	self		assert: client varA		equals: value.	self		assert: client varB		equals: value! !

!RsrMessageSendingTest methodsFor!
testRemoteError	| client server exception |	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [Error new tag: 'tag'; messageText: 'messageText'; signal].	[client value]		on: RsrRemoteError		do: [:ex | exception := ex. ex return].	self		assert: exception class		equals: RsrRemoteError.	self		assert: exception originalClassName		equals: #Error.	self		assert: exception tag		equals: 'tag'.	self		assert: exception messageText		equals: 'messageText'.	self		assert: exception stack isString;		assert: exception stack size > 0! !

!RsrMessageSendingTest methodsFor!
testRemoteErrorWithTag	| client server tag messageText exception |	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	tag := nil.	messageText := 'messageText'.	server action: [Error new tag: tag; messageText: messageText; signal].	exception := [client value]		on: RsrRemoteError		do: [:ex | ex return: ex].	self		assert: exception class		equals: RsrRemoteError.	self		assert: exception originalClassName		equals: #Error.	self		assert: exception tag		equals: 'messageText'.	self		assert: exception messageText		equals: 'messageText'.	self		assert: exception stack isString;		assert: exception stack size > 0.	tag := 42.	exception := [client value]		on: RsrRemoteError		do: [:ex | ex return: ex].	self		assert: exception tag		equals: '42'.	tag := RsrSignalErrorInAsString new.	exception := [client value]		on: RsrRemoteError		do: [:ex | ex return: ex].	self		assert: exception tag		equals: 'Unable to pack #tag containing an instance of RsrSignalErrorInAsString'! !

!RsrValueHolder methodsFor!
value	^value! !

!RsrValueHolder methodsFor!
value: anObject	value := anObject.	self synchronize! !

!RsrSocketStreamTestCase methodsFor!
tearDown	serverStream close.	clientStream close.	super tearDown! !

!RsrSocketStreamTestCase methodsFor!
setUp	super setUp.	self initializeStreams! !

!RsrSocketStreamTestCase methodsFor!
initializeStreams	| listener client server |	listener := RsrSocket new.	client := RsrSocket new.	listener listenOn: self listenPort.	client		connectToHost: '127.0.0.1'		port: self listenPort.	server := listener accept.	listener close.	serverStream := RsrSocketStream on: server.	clientStream := RsrSocketStream on: client! !

!RsrSocketStreamTestCase methodsFor!
testClose	serverStream close.	self		deny: serverStream isConnected;		assert: clientStream isConnected.	self		should: [clientStream next]		raise: RsrSocketClosed.	self		deny: clientStream isConnected! !

!RsrSocketStreamTestCase methodsFor!
listenPort	^47856! !

!RsrRemoteActionServer methodsFor!
action: aBlock	action := aBlock! !

!RsrRemoteActionServer methodsFor!
value	^self action value! !

!RsrRemoteActionServer methodsFor!
action	^action! !

!RsrRemoteActionServer methodsFor!
value: anObject	^self action value: anObject! !

!RsrRemoteActionServer methodsFor!
valueWithArguments: anArray	^self action valueWithArguments: anArray! !

!RsrReflectedVariableTestServiceA methodsFor!
varA	^varA! !

!RsrTestService methodsFor!
sharedVariable: anObject	sharedVariable := anObject! !

!RsrTestService methodsFor!
remoteSelf	^remoteSelf! !

!RsrTestService methodsFor!
sharedVariable	^sharedVariable! !

!RsrReflectedVariableTestServiceB methodsFor!
varB	^varB! !

!RsrDecoderTest methodsFor!
decodeService: anObjectBytes	^self decoder decodeService: anObjectBytes readStream! !

!RsrDecoderTest methodsFor!
testDeliverErrorResponse	| encoding command error |	encoding :=		#[0 0 0 0 0 0 0 4], "DeliverErrorResponse Command"		#[0 0 0 0 0 0 0 1], "Transaction Id"		#[0 0 0 0 0 0 0 0], "originalClassName: Symbol encoding"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 5], "Length of UTF-8 bytes"		#[69 114 114 111 114], "#Error"		#[0 0 0 0 0 0 0 0], "OID = 0"		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"		#[0 0 0 0 0 0 0 3], "length"		#[116 97 103],	 "tag"		#[0 0 0 0 0 0 0 0], "OID = 0"		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"		#[0 0 0 0 0 0 0 12], "length"		#[109 101 115 115 97 103 101 32 116 101 120 116],	 "message text"		#[0 0 0 0 0 0 0 0], "OID = 0"		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"		#[0 0 0 0 0 0 0 10], "length"		#[115 116 97 99 107 32 100 117 109 112].	 "stack dump"	command := self decoder decodeCommand: encoding readStream.	self		assert: command class		equals: RsrDeliverErrorResponse.	self		assert: command transaction		equals: 1.	error := command remoteError.	self		assert: error class		equals: RsrRemoteError.	self		assert: error originalClassName		equals: #Error.	self		assert: error tag		equals: 'tag'.	self		assert: error messageText		equals: 'message text'.	self		assert: error stack		equals: 'stack dump'! !

!RsrDecoderTest methodsFor!
testServiceDecodeIdentity	"Ensure that decoding an object multiple times results in	a single object getting created."	| firstService secondService |	firstService := self decodeService: self serviceNoInstVarsEncoding.	secondService := self decodeService: self serviceNoInstVarsEncoding.	self		assert: firstService		identicalTo: secondService! !

!RsrDecoderTest methodsFor!
testServiceReferenceService	| rootService referencedService |	referencedService := self decodeService: self referencedServiceEncoding.	self		assert: referencedService class		equals: RsrServerNoInstVars.	self		assert: referencedService _id		equals: 2.	rootService := self decodeService: self rootServiceEncoding.	self		assert: rootService class		equals: RsrServerReferenceService.	self		assert: rootService service		equals: referencedService! !

!RsrDecoderTest methodsFor!
testRetainObject	| encoding command |	encoding :=		#[0 0 0 0 0 0 0 0], "RetainObject Command"		#[0 0 0 0 0 0 0 0], "ServiceType Object"		#[0 0 0 0 0 0 0 1], "Service OID"		#[0 0 0 0 0 0 0 0], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 19], "Length of UTF-8 bytes"		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"	command := self decoder decodeCommand: encoding readStream.	self		assert: command object class		equals: RsrServerNoInstVars.	self		assert: command object _id		equals: 1.	self assert: (registry includesKey: command object _id)! !

!RsrDecoderTest methodsFor!
testDeliverResponse	| response encoding command |	response := #responseSymbol.	encoding :=		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"		#[0 0 0 0 0 0 0 1], "Transaction Id"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 14], "Length of UTF-8 bytes"		#[114 101 115 112 111 110 115 101 83 121 109 98 111 108]. "#responseSymbol"	command := self decoder decodeCommand: encoding readStream.	self		assert: command class		equals: RsrDeliverResponse.	self		assert: command transaction		equals: 1.	self		assert: command response		equals: response! !

!RsrDecoderTest methodsFor!
verifyImmediate: expectedencoding: encoding	| actual |	actual := self decoder decodeObjectReference: encoding readStream.	self		assert: actual		equals: expected! !

!RsrDecoderTest methodsFor!
testServiceNoInstVars	| decodedService |	decodedService := self decodeService: self serviceNoInstVarsEncoding.	self		assert: decodedService class		equals: RsrServerNoInstVars.	self		assert: decodedService _id		equals: 1! !

!RsrDecoderTest methodsFor!
assertReference: bytesdecodesTo: expected	| actual |	actual := self decoder decodeObjectReference: bytes readStream.	self		assert: actual		equals: expected! !

!RsrDecoderTest methodsFor!
verifyControlWord: expectedencoding: bytes	| actual |	actual := self decoder decodeControlWord: bytes readStream.	self		assert: actual		equals: expected! !

!RsrDecoderTest methodsFor!
testSendMessage	| service encoding command |	service := RsrServerNoInstVars		_id: 1		connection: RsrMockConnection new.	self decoder.	registry		serviceAt: 1		put: service.	encoding :=		#[0 0 0 0 0 0 0 1], "SendMessage Command"		#[0 0 0 0 0 0 0 1], "Transaction ID"		#[0 0 0 0 0 0 0 0], "Argument Count"		#[0 0 0 0 0 0 0 1], "Receiver OID"		#[0 0 0 0 0 0 0 0], "Selector Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 8], "Length of UTF-8 bytes"		#[114 101 116 117 114 110 52 50]. "#return42"	command := self decoder decodeCommand: encoding readStream.	self		assert: command class		equals: RsrSendMessage.	self		assert: command transaction		equals: 1.	self		assert: command receiver		identicalTo: service.	self		assert: command selector		identicalTo: #return42.	self		assert: command arguments		equals: #()! !

!RsrDecoderTest methodsFor!
testReleaseObjects	| command encoding |	encoding :=		#[0 0 0 0 0 0 0 3], "ReleaseObjects Command"		#[0 0 0 0 0 0 0 5], "Num OIDS"		#[0 0 0 0 0 0 0 1], "First OID"		#[0 0 0 0 0 0 0 2],		#[0 0 0 0 0 0 0 3],		#[0 0 0 0 0 0 0 4],		#[0 0 0 0 0 0 0 5]. "Last OID"	command := self decoder decodeCommand: encoding readStream.	self		assert: command oids		equals: #(1 2 3 4 5)! !

!RsrReflectedVariableTestServer methodsFor!
setVarsToAndReturn: anObject	^varA := varB := anObject! !

!RsrServiceTest methodsFor!
testCreateServiceWithDistinctClientAbstractService	| client |	client := self serviceFactoryA serviceFor: #RsrRemoteAction.	self		assert: client class		equals: RsrRemoteAction clientClass! !

!RsrServiceTest methodsFor!
mirror: aService	^(connectionA serviceFor: #RsrClientNoInstVars) sendReturnArgument: aService! !

!RsrServiceTest methodsFor!
testVariableReflection	| localService remoteService |	localService := RsrTestService clientClass new		sharedVariable: #shared;		privateVariable: #private;		yourself.	self mirror: localService.	remoteService := connectionB registry serviceAt: localService _id.	self		assert: localService sharedVariable		identicalTo: remoteService sharedVariable.	self		assert: localService privateVariable		identicalTo: #private.	self		assert: remoteService privateVariable		identicalTo: nil! !

!RsrServiceTest methodsFor!
testEnsureServersAreCachedAndReused	| client service1 service2 |	client := connectionA serviceFor: #RsrRemoteAction.	service1 := connectionB registry serviceAt: client _id.	self mirror: client.	service2 := connectionB registry serviceAt: client _id.	self		assert: service1		identicalTo: service2! !

!RsrServiceTest methodsFor!
testMessagesDispatchedSeriallyForMultipleServices	"Ensure a long-running process in one Service will not cause startvation of other Services"	| delayClient delayServer client server initialCounter counter delayedResult delaySem immediateResult |	delayClient := RsrConcurrentTestClient new.	client := RsrConcurrentTestClient new.	self mirror: delayClient.	self mirror: client.	initialCounter := 0.	counter := Array with: initialCounter.	delayServer := connectionB registry serviceAt: delayClient _id.	server := connectionB registry serviceAt: client _id.	delaySem := Semaphore new.	delayServer		counter: counter;		semaphore: delaySem.	server counter: counter.	self		assumption: 'The call to #delayedCounter needs to run before the methods continues.';		assumption: 'The delay period in #delayedCounter will be enough to ensure #counterWithIncrement processes'.	RsrProcessModel fork: [delayedResult := delayClient delayedCounter. delaySem signal].	delaySem wait.	immediateResult := client counterWithIncrement.	self		assert: immediateResult		equals: initialCounter.	delaySem wait.	self		assert: delayedResult		equals: initialCounter! !

!RsrServiceTest methodsFor!
testRegisterWith	| instance |	instance := RsrRemoteAction clientClass new.	self deny: instance isMirrored.	instance registerWith: connectionA.	self assert: instance isMirrored! !

!RsrServiceTest methodsFor!
testReflectedVariableNames	| client server clientNames serverNames |	client := connectionA serviceFor: #RsrTestService.	server := connectionB registry serviceAt: client _id.	clientNames := client reflectedVariableNames.	serverNames := server reflectedVariableNames.	self		assert: clientNames		equals: serverNames.	self		assert: clientNames size		equals: 1.	self		assert: (clientNames at: 1) asSymbol		equals: #sharedVariable.	client := connectionA serviceFor: #RsrReflectedVariableTestServiceB.	server := connectionB registry serviceAt: client _id.	clientNames := client reflectedVariableNames.	serverNames := server reflectedVariableNames.	self		assert: clientNames		equals: serverNames.	self		assert: clientNames size		equals: 2.	self		assert: (clientNames at: 1) asSymbol		equals: #varA.	self		assert: (clientNames at: 2) asSymbol		equals: #varB! !

!RsrServiceTest methodsFor!
testIsMirrored	| instance |	instance := RsrRemoteAction clientClass new.	self deny: instance isMirrored.	self mirror: instance.	self assert: instance isMirrored! !

!RsrServiceTest methodsFor!
testHasRemoteSelf	| service |	service := RsrTestService clientClass new.	self mirror: service.	self deny: nil == service remoteSelf! !

!RsrServiceTest methodsFor!
testInitialization	| instance |	instance := RsrRemoteAction clientClass new.	self		assert: instance isMirrored		equals: false.	self		assert: instance _id		equals: nil.	self		assert: instance _connection		equals: nil! !

!RsrServiceTest methodsFor!
testMessageDispatchedSeriallyAndToSameProcessForSingleService	"Ensure that when a message is sent to a Service it is always dispatched to the same process"	| client server process1 process2 |	client := self mirror: RsrConcurrentTestClient new.	server := connectionB registry serviceAt: client _id.	client stashProcess.	process1 := server stashedProcess.	client stashProcess.	process2 := server stashedProcess.	self		assert: process1		identicalTo: process2! !

!RsrServiceTest methodsFor!
testCreateServiceWithSameClientAbstractService	| client server |	client := self serviceFactoryA serviceFor: #RsrSameTemplateAndClientService.	self		assert: client class		equals: RsrSameTemplateAndClientService.	server := connectionB registry serviceAt: client _id.	self		assert: server replicated1		equals: nil.	self		assert: server replicated2		equals: nil.	client		replicated1: 1;		replicated2: 2;		synchronize.	self		assert: server replicated1		equals: 1.	self		assert: server replicated2		equals: 2.	server		replicated1: 10;		replicated2: 20;		private1: 3;		synchronize.	self		assert: client replicated1		equals: 10.	self		assert: client replicated2		equals: 20! !

!RsrMockEncoder methodsFor!
encodeRetainObject: anObject	^ByteArray new! !

!RsrMockEncoder methodsFor!
encode: anObject	^RsrRetainObject		object: anObject		encoding: ByteArray new! !

!RsrMockEncoder methodsFor!
encodeObject: anObject	^ByteArray new! !

!RsrServerTestService methodsFor!
privateVariable	^privateVariable! !

!RsrServerTestService methodsFor!
privateVariable: anObject	privateVariable := anObject! !

!RsrRemoteActionClient methodsFor!
value	^remoteSelf value! !

!RsrRemoteActionClient methodsFor!
valueWithArguments: anArray	^remoteSelf valueWithArguments: anArray! !

!RsrRemoteActionClient methodsFor!
value: anObject	^remoteSelf value: anObject! !

!RsrNumericSpigotTest methodsFor!
testNext	| spigot |	spigot := self spigotClass naturals.	self		assert: (Array with: 1 with: 2 with: 3)		equals: (spigot next: 3)! !

!RsrNumericSpigotTest methodsFor!
spigotClass	^RsrNumericSpigot! !

!RsrNumericSpigotTest methodsFor!
testNaturals	| spigot |	spigot := self spigotClass naturals.	self		assert: spigot next		equals: 1.	self		assert: spigot next		equals: 2! !

!RsrNumericSpigotTest methodsFor!
testDefault	| spigot |	spigot := self spigotClass new.	self		assert: spigot next		equals: 0.	self		assert: spigot next		equals: 1! !

!RsrNumericSpigotTest methodsFor!
testNegativeStep	| spigot |	spigot := self spigotClass		start: 0		step: -1.	self		assert: spigot next		equals: 0.	self		assert: spigot next		equals: -1.	self		assert: spigot next		equals: -2! !

!RsrNumericSpigotTest methodsFor!
testFloat	| spigot |	spigot := self spigotClass		start: 0		step: 0.5.	self		assert: spigot next		equals: 0.	self		assert: spigot next		equals: 0.5.	self		assert: spigot next		equals: 1.0.! !

!RsrCodecTest methodsFor!
serviceNoInstVarsEncoding	^#[0 0 0 0 0 0 0 0], "type"	#[0 0 0 0 0 0 0 1], "rootService's OID = 1"	#[0 0 0 0 0 0 0 0], "Inst Var Count"	#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"	#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"	#[0 0 0 0 0 0 0 19], "Length of UTF-8 encoded bytes"	#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115] "#RsrServerNoInstVars"! !

!RsrCodecTest methodsFor!
testDateTime	| dt encoding |	dt := RsrDateAndTimeSpecies posixEpoch.	encoding :=		#[0 0 0 0 0 0 0 0],		#[0 0 0 0 0 0 0 14],		#[0 0 0 0 0 0 0 0].	self		verifyImmediate: dt		encoding: encoding.	dt := RsrDateAndTimeSpecies fromMicroseconds: 1562692562657612. "2019-07-09T10:16:02.657612-07:00"	encoding :=		#[0 0 0 0 0 0 0 0],		#[0 0 0 0 0 0 0 14],		#[0 5 141 66 183 23 33 76].	self		verifyImmediate: dt		encoding: encoding.	dt := RsrDateAndTimeSpecies fromMicroseconds: -1000000. "1969-12-31T23:59:59-00:00"	encoding :=		#[0 0 0 0 0 0 0 0],		#[0 0 0 0 0 0 0 14],		#[255 255 255 255 255 240 189 192].	self		verifyImmediate: dt		encoding: encoding.	dt := RsrDateAndTimeSpecies fromMicroseconds: -491277642567488. "1954-06-07T14:59:17.432512-07:00"	encoding :=		#[0 0 0 0 0 0 0 0],		#[0 0 0 0 0 0 0 14],		#[255 254 65 47 130 160 240 192].	self		verifyImmediate: dt		encoding: encoding! !

!RsrCodecTest methodsFor!
decoder	^decoder ifNil: [decoder := RsrDecoder registry: (registry := RsrRegistry new) connection: RsrMockConnection new]! !

!RsrCodecTest methodsFor!
verifyControlWord: anIntegerencoding: bytes	self subclassResponsibility! !

!RsrCodecTest methodsFor!
genericSymbol	^#genericSymbol! !

!RsrCodecTest methodsFor!
testArray	| array encoding |	array := Array		with: self genericSymbol		with: 5		with: nil.	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate Object OID"		#[0 0 0 0 0 0 0 9], "Array type"		#[0 0 0 0 0 0 0 3], "3 elements"		self genericSymbolEncoding, "Generic Symbol"		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 3], "Positive Integer"		#[0 0 0 0 0 0 0 1], "num bytes"		#[5], "5"		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 6].	self		verifyImmediate: array		encoding: encoding.	array := Array new.	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 9], "Array type"		#[0 0 0 0 0 0 0 0].	self		verifyImmediate: array		encoding: encoding! !

!RsrCodecTest methodsFor!
verifyImmediate: anImmediateObjectencoding: encoding	self subclassResponsibility! !

!RsrCodecTest methodsFor!
testString	| encoding |	encoding :=		#[0 0 0 0 0 0 0 0], "OID = 0"		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"		#[0 0 0 0 0 0 0 0], "length"		#[].	 "empty string"	self		verifyImmediate: ''		encoding: encoding.	encoding :=		#[0 0 0 0 0 0 0 0], "OID = 0"		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"		#[0 0 0 0 0 0 0 13], "length"		#[103 101 110 101 114 105 99 83 116 114 105 110 103].	 "genericString"	self		verifyImmediate: 'genericString'		encoding: encoding! !

!RsrCodecTest methodsFor!
testByteArray	| bytes encoding |	bytes := #[].	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate Object OID"		#[0 0 0 0 0 0 0 10], "ByteArray type"		#[0 0 0 0 0 0 0 0], "size"		bytes.	self		verifyImmediate: bytes		encoding: encoding.	bytes := #[1 2 3 4 5].	encoding :=		#[0 0 0 0 0 0 0 0],		#[0 0 0 0 0 0 0 10],		#[0 0 0 0 0 0 0 5],		bytes.	self		verifyImmediate: bytes		encoding: encoding! !

!RsrCodecTest methodsFor!
rootServiceEncoding	^#[0 0 0 0 0 0 0 0], "type"	#[0 0 0 0 0 0 0 1], "rootService's OID = 1"	#[0 0 0 0 0 0 0 1], "Inst Var Count"	#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"	#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"	#[0 0 0 0 0 0 0 25], "Length of UTF-8 encoded bytes"	#[82 115 114 83 101 114 118 101 114 82 101 102 101 114 101 110 99 101 83 101 114 118 105 99 101],	#[0 0 0 0 0 0 0 2]. "#RsrServerReferenceService"! !

!RsrCodecTest methodsFor!
referencedServiceEncoding	^#[0 0 0 0 0 0 0 0], "type"	#[0 0 0 0 0 0 0 2], "referencedService's OID = 2"	#[0 0 0 0 0 0 0 0], "Inst Var Count"	#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"	#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"	#[0 0 0 0 0 0 0 19], "Length of UTF-8 encoded bytes"	#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"! !

!RsrCodecTest methodsFor!
encoder	^RsrEncoder new! !

!RsrCodecTest methodsFor!
testSet	| set encoding result |	set := Set new.	encoding :=		#[0 0 0 0 0 0 0 0], "OID"		#[0 0 0 0 0 0 0 11], "Set"		#[0 0 0 0 0 0 0 0]. "0 elements"	self		verifyImmediate: set		encoding: encoding.	set := Set		with: true		with: nil.	encoding := self encodeReferenceOf: set.	result := self decoder decodeObjectReference: encoding readStream.	self		assert: result		equals: set.	self		deny: result		identicalTo: set.	"self hack: 'Hashed collections do not have an ordering'.	encoding :=		#[0 0 0 0 0 0 0 0], ""OID""		#[0 0 0 0 0 0 0 11], ""Set""		#[0 0 0 0 0 0 0 2], ""2 elements""		#[0 0 0 0 0 0 0 0], ""true""		#[0 0 0 0 0 0 0 7],		#[0 0 0 0 0 0 0 0], ""nil""		#[0 0 0 0 0 0 0 6].	self		verifyImmediate: set		encoding: encoding"! !

!RsrCodecTest methodsFor!
encodeReferenceOf: anObject	^ByteArray streamContents: [:stream | self encoder encodeReferenceOf: anObject onto: stream]! !

!RsrCodecTest methodsFor!
testDictionary	| dictionary encoding result |	dictionary := Dictionary new.	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate Object OID"		#[0 0 0 0 0 0 0 13], "Dictionary type"		#[0 0 0 0 0 0 0 0]. "0 associations"	self		verifyImmediate: dictionary		encoding: encoding.	dictionary := Dictionary new		at: 1 put: self genericSymbol;		at: false put: true;		yourself.	encoding := self encodeReferenceOf: dictionary.	result := self decoder decodeObjectReference: encoding readStream.	self		assert: result		equals: dictionary.	self		deny: result		identicalTo: dictionary.	"self hack: 'Order is not guaranteed in a dictionary'.	encoding :=		#[0 0 0 0 0 0 0 0], ""Immediate OID""		#[0 0 0 0 0 0 0 13], ""Dictionary Type""		#[0 0 0 0 0 0 0 2], ""Two assocs""		#[0 0 0 0 0 0 0 0], ""nil""		#[0 0 0 0 0 0 0 6],		#[0 0 0 0 0 0 0 0], ""true""		#[0 0 0 0 0 0 0 7],		#[0 0 0 0 0 0 0 0], ""Integer 1""		#[0 0 0 0 0 0 0 3],		#[0 0 0 0 0 0 0 1],		#[1],		self genericSymbolEncoding.	self		verifyImmediate: dictionary		encoding: encoding"! !

!RsrCodecTest methodsFor!
genericSymbolEncoding	^#[0 0 0 0 0 0 0 0], "OID = 0"	#[0 0 0 0 0 0 0 1], "Immediate Type = 1"	#[0 0 0 0 0 0 0 13], "Length of UTF-8 data"	#[103 101 110 101 114 105 99 83 121 109 98 111 108]	"#genericSymbol"! !

!RsrCodecTest methodsFor!
testOrderedCollection	| oc encoding |	oc := OrderedCollection new.	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 12], "OrderedCollection type"		#[0 0 0 0 0 0 0 0].	self		verifyImmediate: oc		encoding: encoding.	oc := OrderedCollection		with: self genericSymbol		with: 5		with: nil.	encoding :=		#[0 0 0 0 0 0 0 0], "Immediate Object OID"		#[0 0 0 0 0 0 0 12], "OrderedCollection type"		#[0 0 0 0 0 0 0 3], "3 elements"		self genericSymbolEncoding, "Generic Symbol"		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 3], "Positive Integer"		#[0 0 0 0 0 0 0 1], "num bytes"		#[5], "5"		#[0 0 0 0 0 0 0 0], "Immediate OID"		#[0 0 0 0 0 0 0 6].	self		verifyImmediate: oc		encoding: encoding! !

!RsrCodecTest methodsFor!
testSymbol	self		verifyImmediate: self genericSymbol		encoding: self genericSymbolEncoding! !

!RsrCodecTest methodsFor!
testControlWord		self		verifyControlWord: 0		encoding: #[0 0 0 0 0 0 0 0].	self		verifyControlWord: 1		encoding: #[0 0 0 0 0 0 0 1].	self		verifyControlWord: -1		encoding: #[255 255 255 255 255 255 255 255].	self		verifyControlWord: (2 raisedTo: 63) - 1		encoding: #[127 255 255 255 255 255 255 255].	self		verifyControlWord: (2 raisedTo: 63) negated		encoding: #[128 0 0 0 0 0 0 0]! !

!RsrClientTestService methodsFor!
privateVariable	^privateVariable! !

!RsrClientTestService methodsFor!
privateVariable: anObject	privateVariable := anObject! !

!RsrMockConnection methodsFor!
_sendMessage: aMessageto: anRsrObject	lastMessage := RsrSendMessage		transaction: 1		receiver: anRsrObject		selector: aMessage selector		arguments: aMessage arguments.	^RsrPromise new		fulfill: nil;		yourself! !

!RsrMockConnection methodsFor!
encoder	^RsrMockEncoder new! !

!RsrMockConnection methodsFor!
oidSpigot	^idSpigot ifNil: [idSpigot := RsrThreadSafeNumericSpigot naturals]! !

!RsrMockConnection methodsFor!
_forwarderClass	^forwarderClass ifNil: [RsrForwarder]! !

!RsrMockConnection methodsFor!
lastMessage	^[lastMessage]		ensure: [lastMessage := nil]! !

!RsrMockConnection methodsFor!
registry	^registry ifNil: [registry := RsrRegistry new]! !

!RsrMockConnection methodsFor!
releaseOid: anInteger! !

!RsrMockConnection methodsFor!
ensureRegistered: aService	aService isMirrored		ifTrue: [^self].	aService		_id: self oidSpigot next		connection: self! !

!RsrMockConnection methodsFor!
forwarderClass: aClass	forwarderClass := aClass! !

!RsrReflectedVariableTestClient methodsFor!
setVarsToAndReturn: anObject	^remoteSelf setVarsToAndReturn: anObject! !

!RsrLifetimeTest methodsFor!
testCloseWithDanglingObject	"It is possible that the connection could disconnect between when an object	is received and when the upcoming SendMessage or DeliverResponse message is received.	If this is the case, we could leak memory due to the caching used to ensure	the object is stored in memory long enough to process the upcoming message.	Test to ensure the object is freed on the connection close."	| service command |	self maximumReclamation.	self assert: RsrClientNoInstVars allInstances isEmpty.	service := RsrServerNoInstVars new.	service		_id: 2		connection: connectionA.	command := RsrRetainObject object: service.	command		encodeUsing: connectionA encoder;		writeUsing: connectionA commandWriter.	connectionA commandWriter flush.	(Delay forMilliseconds: 10) wait.	connectionA close.	connectionB close.	service := command := nil.	self maximumReclamation.	self assert: RsrClientNoInstVars allInstances isEmpty! !

!RsrLifetimeTest methodsFor!
testRemoteReferenceLifetime	| valueServiceLocal valueServiceRemote serviceLocal serviceRemote id marker actual |	serviceLocal := RsrClientNoInstVars new.	valueServiceLocal := connectionA serviceFor: #RsrValueHolderClient.	valueServiceLocal value: serviceLocal.	valueServiceRemote := connectionB registry serviceAt: valueServiceLocal _id.	serviceRemote := valueServiceRemote value.	id := serviceLocal _id.	self		assert: serviceRemote class		equals: RsrServerNoInstVars.	serviceLocal := serviceRemote := nil.	valueServiceRemote value: nil.	self maximumReclamation.	(Delay forSeconds: 1) wait. "Needed to ensure there is time for release to propogate to remote environment."	marker := Object new.	actual := connectionA registry serviceAt: id ifAbsent: [marker].	self		assert: actual		equals: marker.	actual := connectionB registry serviceAt: id ifAbsent: [marker].	self		assert: actual		equals: marker! !

!RsrLifetimeTest methodsFor!
testObjectCreatedViaServiceFactory	"Ensure objects created by the RsrServiceFactory maintain the same	properties as other objects"	| client server remoteService |	client := connectionA serviceFor: #RsrRemoteAction.	server := connectionB registry serviceAt: client _id.	server action: [:object | object].	self		assert: (connectionA registry serviceAt: client _id)		identicalTo: client.	remoteService := connectionB registry serviceAt: client _id.	self		assert: remoteService class		equals: RsrRemoteActionServer.	self		assert: (client value: client)		identicalTo: client! !