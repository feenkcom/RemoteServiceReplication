| package |
package := Package name: 'RemoteServiceReplication-Test'.
package paxVersion: 1; basicComment: ''.

package classNames
	add: #RsrThreadSafeNumericSpigotTest;
	add: #RsrDecoderTest;
	add: #RsrEncoderTest;
	add: #RsrReflectedVariableTestServer;
	add: #RsrForwarderTest;
	add: #RsrMockEncoder;
	add: #RsrMockConnection;
	add: #RsrServiceTest;
	add: #RsrAbstractReflectedVariableTestServiceA;
	add: #RsrSocketStreamTestCase;
	add: #RsrClientAllDataObjects;
	add: #RsrServerNoInstVars;
	add: #RsrPromiseTest;
	add: #RsrSpeciesReturnEquality;
	add: #RsrAbstractTestService;
	add: #RsrNumericSpigotTest;
	add: #RsrAbstractReferenceAllSpecies;
	add: #RsrMockRegistry;
	add: #RsrMessageSendingTest;
	add: #RsrAbstractReflectedVariableTestServiceB;
	add: #RsrSystemTestCase;
	add: #RsrServerAllDataObjects;
	add: #RsrValueHolderClient;
	add: #RsrServiceAbstractReferenceService;
	add: #RsrClientTestService;
	add: #RsrReferenceAllSpeciesClient;
	add: #RsrCodecTest;
	add: #RsrServerTestService;
	add: #RsrClientReferenceService;
	add: #RsrReflectedVariableTestClient;
	add: #RsrLifetimeTest;
	add: #RsrServiceAbstractNoInstVars;
	add: #RsrValueHolderServer;
	add: #RsrReferenceAllSpeciesServer;
	add: #RsrRetainAnalysisTest;
	add: #RsrServiceAbstractAllDataObjects;
	add: #RsrAbstractValueHolderService;
	add: #RsrServerReferenceService;
	add: #RsrClientNoInstVars;
	yourself.

package methodNames
	yourself.


package setPrerequisites: #(
	'RemoteServiceReplication-Base.pac'
	'RemoteServiceReplication-Compatibility-Dolphin.pac'
	'RemoteServiceReplication.pac'
	'RemoteServiceReplication-Compatibility-Test.pac'
).

package!

RsrObject
	subclass: #RsrMockConnection
	instanceVariableNames: 'forwarderClass lastMessage registry idSpigot'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrMockConnection categoriesForClass!RemoteServiceReplication-Test-Mocks! !

RsrObject
	subclass: #RsrMockRegistry
	instanceVariableNames: 'objects idSpigot'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrMockRegistry categoriesForClass!RemoteServiceReplication-Test-Mocks! !

RsrService
	subclass: #RsrAbstractReferenceAllSpecies
	instanceVariableNames: 'emptyService serviceReferenceService emptySymbol symbol emptyString string largeNegative smallNegative zero smallPositive largePositive asciiCharacter undefinedObject booleanTrue booleanFalse emptyArray array emptyByteArray byteArray emptyOrderedCollection orderedCollection emptyDictionary dictionary epoch beforeEpoch afterEpoch arrayWithAllTypes emptySet set setWithAllTypes orderedCollectionWithAllTypes dictionaryWithAllTypes unicodeCharacter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrAbstractReferenceAllSpecies categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrService
	subclass: #RsrAbstractReflectedVariableTestServiceA
	instanceVariableNames: 'varA'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrAbstractReflectedVariableTestServiceA categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrService
	subclass: #RsrAbstractTestService
	instanceVariableNames: 'sharedVariable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrAbstractTestService categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrService
	subclass: #RsrAbstractValueHolderService
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrAbstractValueHolderService categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrService
	subclass: #RsrServiceAbstractAllDataObjects
	instanceVariableNames: 'positiveSmallInteger negativeSmallInteger integerZero string'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceAbstractAllDataObjects categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrService
	subclass: #RsrServiceAbstractNoInstVars
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceAbstractNoInstVars categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrService
	subclass: #RsrServiceAbstractReferenceService
	instanceVariableNames: 'service'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceAbstractReferenceService categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrAbstractReflectedVariableTestServiceA
	subclass: #RsrAbstractReflectedVariableTestServiceB
	instanceVariableNames: 'varB'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrAbstractReflectedVariableTestServiceB categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrServiceAbstractAllDataObjects
	subclass: #RsrClientAllDataObjects
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClientAllDataObjects categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrServiceAbstractNoInstVars
	subclass: #RsrClientNoInstVars
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClientNoInstVars categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrServiceAbstractReferenceService
	subclass: #RsrClientReferenceService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClientReferenceService categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrAbstractTestService
	subclass: #RsrClientTestService
	instanceVariableNames: 'privateVariable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClientTestService categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrTestCase
	subclass: #RsrCodecTest
	instanceVariableNames: 'registry decoder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrCodecTest comment: 'This class contains tests'!
!RsrCodecTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrTestCase
	subclass: #RsrForwarderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrForwarderTest comment: 'This class contains tests'!
!RsrForwarderTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrEncoder
	subclass: #RsrMockEncoder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrMockEncoder categoriesForClass!RemoteServiceReplication-Test-Mocks! !

RsrTestCase
	subclass: #RsrNumericSpigotTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrNumericSpigotTest comment: 'This class contains tests'!
!RsrNumericSpigotTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrTestCase
	subclass: #RsrPromiseTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrPromiseTest comment: 'This class contains tests'!
!RsrPromiseTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrAbstractReferenceAllSpecies
	subclass: #RsrReferenceAllSpeciesClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrReferenceAllSpeciesClient categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrAbstractReferenceAllSpecies
	subclass: #RsrReferenceAllSpeciesServer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrReferenceAllSpeciesServer categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrTestCase
	subclass: #RsrRetainAnalysisTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrRetainAnalysisTest comment: 'This class contains tests'!
!RsrRetainAnalysisTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrServiceAbstractAllDataObjects
	subclass: #RsrServerAllDataObjects
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServerAllDataObjects categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrServiceAbstractNoInstVars
	subclass: #RsrServerNoInstVars
	instanceVariableNames: 'marker'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServerNoInstVars categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrServiceAbstractReferenceService
	subclass: #RsrServerReferenceService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServerReferenceService categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrAbstractTestService
	subclass: #RsrServerTestService
	instanceVariableNames: 'privateVariable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServerTestService categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrTestCase
	subclass: #RsrSocketStreamTestCase
	instanceVariableNames: 'serverStream clientStream'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSocketStreamTestCase comment: 'This class contains tests'!
!RsrSocketStreamTestCase categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrTestCase
	subclass: #RsrSystemTestCase
	instanceVariableNames: 'connectionA connectionB'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSystemTestCase comment: 'This class contains tests'!
!RsrSystemTestCase categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrAbstractValueHolderService
	subclass: #RsrValueHolderClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrValueHolderClient categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrAbstractValueHolderService
	subclass: #RsrValueHolderServer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrValueHolderServer categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrCodecTest
	subclass: #RsrDecoderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrDecoderTest comment: 'This class contains tests'!
!RsrDecoderTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrCodecTest
	subclass: #RsrEncoderTest
	instanceVariableNames: 'connection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrEncoderTest comment: 'This class contains tests'!
!RsrEncoderTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrSystemTestCase
	subclass: #RsrLifetimeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrLifetimeTest comment: 'This class contains tests'!
!RsrLifetimeTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrSystemTestCase
	subclass: #RsrMessageSendingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrMessageSendingTest comment: 'This class contains tests'!
!RsrMessageSendingTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrAbstractReflectedVariableTestServiceB
	subclass: #RsrReflectedVariableTestClient
	instanceVariableNames: 'private'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrReflectedVariableTestClient categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrAbstractReflectedVariableTestServiceB
	subclass: #RsrReflectedVariableTestServer
	instanceVariableNames: 'private'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrReflectedVariableTestServer categoriesForClass!RemoteServiceReplication-Test-Services! !

RsrSystemTestCase
	subclass: #RsrServiceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrServiceTest comment: 'This class contains tests'!
!RsrServiceTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrSystemTestCase
	subclass: #RsrSpeciesReturnEquality
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSpeciesReturnEquality comment: 'This class contains tests'!
!RsrSpeciesReturnEquality categoriesForClass!RemoteServiceReplication-Test-TestCases! !

RsrNumericSpigotTest
	subclass: #RsrThreadSafeNumericSpigotTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrThreadSafeNumericSpigotTest comment: 'This class contains tests'!
!RsrThreadSafeNumericSpigotTest categoriesForClass!RemoteServiceReplication-Test-TestCases! !

!RsrServiceAbstractNoInstVars class methodsFor!
clientClassName	^#RsrClientNoInstVars! !

!RsrServiceAbstractNoInstVars class methodsFor!
serverClassName	^#RsrServerNoInstVars! !

!RsrSystemTestCase class methodsFor!
isAbstract	^self == RsrSystemTestCase! !

!RsrAbstractReferenceAllSpecies class methodsFor!
clientClassName	^#RsrReferenceAllSpeciesClient! !

!RsrAbstractReferenceAllSpecies class methodsFor!
serverClassName	^#RsrReferenceAllSpeciesServer! !

!RsrServiceAbstractReferenceService class methodsFor!
clientClassName	^#RsrClientReferenceService! !

!RsrServiceAbstractReferenceService class methodsFor!
service: aService	^self new		service: aService;		yourself! !

!RsrServiceAbstractReferenceService class methodsFor!
serverClassName	^#RsrServerReferenceService! !

!RsrAbstractValueHolderService class methodsFor!
clientClassName	^#RsrValueHolderClient! !

!RsrAbstractValueHolderService class methodsFor!
value: anRsrObject	^self new		value: anRsrObject;		yourself! !

!RsrAbstractValueHolderService class methodsFor!
serverClassName	^#RsrValueHolderServer! !

!RsrServiceAbstractAllDataObjects class methodsFor!
clientClassName	^#RsrClientAllDataObjects! !

!RsrServiceAbstractAllDataObjects class methodsFor!
serverClassName	^#RsrServerAllDataObjects! !

!RsrMockConnection class methodsFor!
forwarderClass: aClass 	^self new		forwarderClass: aClass;		yourself! !

!RsrAbstractTestService class methodsFor!
clientClassName	^#RsrClientTestService! !

!RsrAbstractTestService class methodsFor!
serverClassName	^#RsrServerTestService! !

!RsrAbstractReflectedVariableTestServiceB class methodsFor!
clientClassName	^#RsrReflectedVariableTestClient! !

!RsrAbstractReflectedVariableTestServiceB class methodsFor!
serverClassName	^#RsrReflectedVariableTestServer! !

!RsrCodecTest class methodsFor!
isAbstract	^self == RsrCodecTest! !

!RsrServerNoInstVars methodsFor!
returnTestSymbol	^#testSymbol! !

!RsrServerNoInstVars methodsFor!
returnNewService	^RsrServerTestService new! !

!RsrServerNoInstVars methodsFor!
setMarker	marker := true! !

!RsrServerNoInstVars methodsFor!
signalError	^Error signal! !

!RsrServerNoInstVars methodsFor!
unaryReturn42	^42! !

!RsrServerNoInstVars methodsFor!
returnObjectNew	^Object new! !

!RsrServerNoInstVars methodsFor!
marker	^marker ifNil: [false]! !

!RsrServerNoInstVars methodsFor!
returnNewServiceInArray	^Array with: RsrServerTestService new! !

!RsrServerNoInstVars methodsFor!
returnSelf	^self! !

!RsrAbstractReferenceAllSpecies methodsFor!
initialize	super initialize.	emptyService := RsrClientNoInstVars new.	serviceReferenceService := RsrClientReferenceService service: RsrClientNoInstVars new.	emptySymbol := #''.	symbol := #Symbol.	emptyString := ''.	string := 'String'.	largeNegative := -879021356918273469872356.	smallNegative := -1.	zero := 0.	smallPositive := 1.	largePositive := 29183756217893561289745689732456.	asciiCharacter := $e.	unicodeCharacter := Character codePoint: 16r01D4.	undefinedObject := nil.	booleanTrue := true.	booleanFalse := false.	emptyArray := #().	array := {emptyService. serviceReferenceService. emptySymbol. symbol. emptyString. string. largeNegative. smallNegative. zero. smallPositive. largePositive. asciiCharacter. unicodeCharacter. undefinedObject. booleanTrue. booleanFalse. emptyArray.}.	emptyByteArray := #[].	byteArray := #[123 45 67].	emptySet := Set new.	set := Set withAll: array.	emptyOrderedCollection := OrderedCollection new.	orderedCollection := OrderedCollection withAll: array.	emptyDictionary := Dictionary new.	dictionary := Dictionary new.	array doWithIndex: [:each :i | dictionary at: i put: each].	beforeEpoch := (RsrDateAndTimeSpecies fromMicroseconds: -491277642567488). "1954-06-07T14:59:17.432512-07:00"	epoch := RsrDateAndTimeSpecies posixEpoch.	afterEpoch := (RsrDateAndTimeSpecies fromMicroseconds: 1562692562657612). "2019-07-09T10:16:02.657612-07:00"	arrayWithAllTypes := array, {emptyArray. array. emptyByteArray. byteArray. emptySet. set. emptyOrderedCollection. orderedCollection. emptyDictionary. dictionary. beforeEpoch. epoch. afterEpoch.}.	setWithAllTypes := Set withAll: arrayWithAllTypes.	orderedCollectionWithAllTypes := OrderedCollection withAll: arrayWithAllTypes.	dictionaryWithAllTypes := Dictionary new.	arrayWithAllTypes doWithIndex: [:each :i | dictionaryWithAllTypes at: i put: each]	! !

!RsrServerTestService methodsFor!
privateVariable	^privateVariable! !

!RsrServerTestService methodsFor!
privateVariable: anObject	privateVariable := anObject! !

!RsrServiceAbstractNoInstVars methodsFor!
sendReturnArgument: anObject	^remoteSelf returnArgument: anObject! !

!RsrServiceAbstractNoInstVars methodsFor!
returnArgument: anObject	^anObject! !

!RsrClientTestService methodsFor!
privateVariable	^privateVariable! !

!RsrClientTestService methodsFor!
privateVariable: anObject	privateVariable := anObject! !

!RsrMockEncoder methodsFor!
encodeRetainObject: anObject	^ByteArray new! !

!RsrMockEncoder methodsFor!
encode: anObject	^RsrRetainObject		object: anObject		encoding: ByteArray new! !

!RsrMockEncoder methodsFor!
encodeObject: anObject	^ByteArray new! !

!RsrRetainAnalysisTest methodsFor!
testServiceAllDataObjects	"While this code is structurally similar to #testClientNoInstVars, it ensures	that Data Objects are actually encoded in-line."	| client registry analysis expected |	client := RsrClientAllDataObjects new.	registry := RsrMockRegistry new.	analysis := self analyze: client.	expected := OrderedCollection		with: (RsrRetainObject object: client encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self assert: client isMirrored! !

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
testServiceWithCycle	"Cycles are disallowed for our POC. Perhaps they will get added later?"	| rootClient referencedClient |	rootClient := RsrClientReferenceService new.	referencedClient := RsrClientReferenceService service: rootClient.	rootClient service: referencedClient.	self assertCycle: rootClient! !

!RsrRetainAnalysisTest methodsFor!
testDictionaryCycle	| dictionary |	dictionary := Dictionary new.	dictionary		at: 1		put: dictionary.	self assertCycle: dictionary.	dictionary removeKey: 1.	dictionary		at: dictionary		put: 1.	self assertCycle: dictionary! !

!RsrRetainAnalysisTest methodsFor!
testNewServicesInDictionary	"Ensure a new service in a collection is properly tagged"	| key value dictionary analysis expected |	key := RsrServerNoInstVars new.	value := RsrServerNoInstVars new.	dictionary := Dictionary new		at: key put: value;		yourself.	analysis := self analyze: dictionary.	expected := OrderedCollection		with: (RsrRetainObject object: key encoding: ByteArray new)		with: (RsrRetainObject object: value encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self		assert: key isMirrored;		assert: value isMirrored! !

!RsrRetainAnalysisTest methodsFor!
testServiceReferencingAnotherService	"While this code is structurally similar to #testClientNoInstVars, it ensures	that Data Objects are actually encoded in-line."	| referencedService client analysis expected |	referencedService := RsrClientNoInstVars new.	client := RsrClientReferenceService service: referencedService.	analysis := self analyze: client.	expected := OrderedCollection		with: (RsrRetainObject object: referencedService encoding: ByteArray new)		with: (RsrRetainObject object: client encoding: ByteArray new).	self		assert: analysis retainCommands		equals: expected.	self		assert: client isMirrored;		assert: referencedService isMirrored! !

!RsrThreadSafeNumericSpigotTest methodsFor!
spigotClass	^RsrThreadSafeNumericSpigot! !

!RsrMessageSendingTest methodsFor!
testReturnNewService	| service returnedService |	service := connectionA serviceFor: #RsrClientNoInstVars.	returnedService := service sendReturnNewService.	self		assert: returnedService class		equals: RsrClientTestService! !

!RsrMessageSendingTest methodsFor!
testRemoteError	| service |	service := connectionA serviceFor: #RsrClientNoInstVars.	self		should: [service sendSignalError]		raise: Error! !

!RsrMessageSendingTest methodsFor!
testReturnInvalidObject	| service |	service := connectionA serviceFor: #RsrClientNoInstVars.	self		should: [service sendReturnObjectNew]		raise: RsrUnsupportedObject! !

!RsrMessageSendingTest methodsFor!
testReturnArgument	| client server arguments dt response |	client := connectionA serviceFor: #RsrClientNoInstVars.	server := connectionB registry at: client _id.	arguments := OrderedCollection new		addAll: #( '' #symbol 'string' $h 0 -14 14 18446744073709551616 -18446744073709551616 nil true false ); 		add: (Character codePoint: 16r259F);		add: (Dictionary new at: 1 put: 2; yourself);		add: (Set with: 14);		add: #[1 2 3 4];		add: (OrderedCollection with: 42 with: 43);		add: #(1 2 #(nil));		yourself.	dt := RsrDateAndTimeSpecies now.	response := client sendReturnArgument: dt.	self		assert: (dt asSeconds * 1000000) rounded		equals: (response asSeconds * 1000000) rounded.	arguments		do:			[:each | | result |			result := client sendReturnArgument: each.			self				assert: result				equals: each].	arguments		do:			[:each | | result |			result := server sendReturnArgument: each.			self				assert: result				equals: each].	self		assert: (client sendReturnArgument: arguments)		equals: arguments.	self		assert: (server sendReturnArgument: arguments)		equals: arguments.	self		assert: (client sendReturnArgument: client)		identicalTo: client.	self		assert: (server sendReturnArgument: server)		identicalTo: server! !

!RsrMessageSendingTest methodsFor!
testSendInvalidObject	| service |	service := connectionA serviceFor: #RsrClientNoInstVars.	self		should: [service sendReturnArgument: Object new]		raise: RsrUnsupportedObject! !

!RsrMessageSendingTest methodsFor!
testReturnNewServiceInArray	| service array returnedService |	service := connectionA serviceFor: #RsrClientNoInstVars.	array := service sendReturnNewServiceInArray.	self		assert: array size		equals: 1.	returnedService := array first.	self		assert: returnedService class		equals: RsrClientTestService! !

!RsrMessageSendingTest methodsFor!
testSendSetMarker	| service remoteService |	service := connectionA serviceFor: #RsrClientNoInstVars.	remoteService := connectionB registry at: service _id.	self deny: remoteService marker.	service sendSetMarker.	self assert: remoteService marker! !

!RsrMessageSendingTest methodsFor!
testReturnSymbol	| service |	service := connectionA serviceFor: #RsrClientNoInstVars.	self		assert: service sendReturnTestSymbol		equals: #testSymbol! !

!RsrServiceAbstractReferenceService methodsFor!
service: anObject	service := anObject! !

!RsrServiceAbstractReferenceService methodsFor!
service	^ service! !

!RsrAbstractValueHolderService methodsFor!
value	^value! !

!RsrAbstractValueHolderService methodsFor!
value: anObject	value := anObject.	self synchronize! !

!RsrServiceTest methodsFor!
testInitialization	| instance |	instance := RsrClientTestService new.	self		assert: instance isMirrored		equals: false.	self		assert: instance _id		equals: nil.	self		assert: instance _connection		equals: nil! !

!RsrServiceTest methodsFor!
testHasRemoteSelf	| service |	service := RsrClientTestService new.	self mirror: service.	self deny: nil == service remoteSelf! !

!RsrServiceTest methodsFor!
testIsMirrored	| instance |	instance := RsrClientTestService new.	self deny: instance isMirrored.	self mirror: instance.	self assert: instance isMirrored! !

!RsrServiceTest methodsFor!
mirror: aService	(connectionA serviceFor: #RsrClientNoInstVars) sendReturnArgument: aService! !

!RsrServiceTest methodsFor!
newInstance	^RsrClientTestService new! !

!RsrServiceTest methodsFor!
testReflectedVariableNames	| clientNames serverNames |	clientNames := RsrClientTestService _variablesToReflect.	serverNames := RsrServerTestService _variablesToReflect.	self		assert: clientNames		equals: serverNames.	self		assert: clientNames size		equals: 1.	self		assert: (clientNames at: 1) asSymbol		equals: #sharedVariable.	clientNames := RsrReflectedVariableTestClient _variablesToReflect.	serverNames := RsrReflectedVariableTestServer _variablesToReflect.	self		assert: clientNames		equals: serverNames.	self		assert: clientNames size		equals: 2.	self		assert: (clientNames at: 1) asSymbol		equals: #varA.	self		assert: (clientNames at: 2) asSymbol		equals: #varB! !

!RsrServiceTest methodsFor!
testVariableReflection	| localService remoteService |	localService := RsrClientTestService new		sharedVariable: #shared;		privateVariable: #private;		yourself.	self mirror: localService.	remoteService := connectionB registry at: localService _id.	self		assert: localService sharedVariable		identicalTo: remoteService sharedVariable.	self		assert: localService privateVariable		identicalTo: #private.	self		assert: remoteService privateVariable		identicalTo: nil! !

!RsrSpeciesReturnEquality methodsFor!
testUnicodeString	self verify: self unicodeString! !

!RsrSpeciesReturnEquality methodsFor!
testServiceWithUnsupportedObject	| service |	service := connectionA serviceFor: #RsrClientNoInstVars.	self		should: [service sendReturnArgument: (RsrValueHolderClient value: Object new)]		raise: RsrUnsupportedObject! !

!RsrSpeciesReturnEquality methodsFor!
testCharacter	self		verify: (Character codePoint: 0);		verify: (Character codePoint: 65);		verify: $A;		verify: (Character codePoint: 16r01D4);		verify: (Character codePoint: 16r8334)! !

!RsrSpeciesReturnEquality methodsFor!
testArray	self		verify: #();		verify: (Array withAll: self basicExamples)! !

!RsrSpeciesReturnEquality methodsFor!
basicExamples	"Give a samples of each species to ensure Collection classes are able to encode each type successfully."	^{RsrClientNoInstVars new.	#h.	#''.	'h'.	''.	0.	234.	-97.	$s.	nil.	true.	false.	{}.	{RsrClientNoInstVars new. {}.}.	#[].	#[123].	Set new.	Set with: 42.	OrderedCollection new.	OrderedCollection with: #x.	Dictionary new.	Dictionary new at: #key put: #value; yourself.	RsrDateAndTimeSpecies posixEpoch.	RsrDateAndTimeSpecies fromMicroseconds: -1000000. "1969-12-31T23:59:59-00:00"}! !

!RsrSpeciesReturnEquality methodsFor!
verify: anObject	"Send <anObject> through RSR and have it returned. Assert it is equivalent."	| result |	result := (connectionA serviceFor: #RsrClientNoInstVars) sendReturnArgument: anObject.	self		assert: result		equals: anObject! !

!RsrSpeciesReturnEquality methodsFor!
testString	self		verify: '';		verify: 'string'! !

!RsrSpeciesReturnEquality methodsFor!
unicodeString	^String		with: $a		with: (Character codePoint: 16r8349)		with: (Character codePoint: 16r10E60)! !

!RsrSpeciesReturnEquality methodsFor!
testByteArray	self		verify: #[];		verify: (ByteArray withAll: (0 to: 255));		verify: (ByteArray new: 1024)! !

!RsrSpeciesReturnEquality methodsFor!
testUnicodeSymbol	self verify: self unicodeString asSymbol! !

!RsrSpeciesReturnEquality methodsFor!
testBoolean	self		verify: true;		verify: false! !

!RsrSpeciesReturnEquality methodsFor!
testUndefinedObject	self verify: nil! !

!RsrSpeciesReturnEquality methodsFor!
testSet	self		verify: Set new;		verify: (Set withAll: self basicExamples)! !

!RsrSpeciesReturnEquality methodsFor!
testDictionary	| example |	example := Dictionary new.	self verify: example.	self basicExamples do: [:each | each ifNotNil: [example at: each put: each]].	example at: #testDictionaryPrivateKey put: nil.	self verify: example! !

!RsrSpeciesReturnEquality methodsFor!
testDateAndTime	self		verify: (RsrDateAndTimeSpecies fromMicroseconds: -491277642567488); "1954-06-07T14:59:17.432512-07:00"		verify: (RsrDateAndTimeSpecies fromMicroseconds: 1562692562657612). "2019-07-09T10:16:02.657612-07:00"! !

!RsrSpeciesReturnEquality methodsFor!
testService	self		verify: RsrClientNoInstVars new;		verify: (RsrClientReferenceService service: RsrClientNoInstVars new);		verify: (RsrServerReferenceService service: RsrClientNoInstVars new);		verify: (RsrReferenceAllSpeciesClient new)! !

!RsrSpeciesReturnEquality methodsFor!
testOrderedCollection	self		verify: OrderedCollection new;		verify: (OrderedCollection withAll: self basicExamples)! !

!RsrSpeciesReturnEquality methodsFor!
testInteger	self		verify: 0;		verify: -1;		verify: 1;		verify: (2 raisedTo: 32);		verify: (2 raisedTo: 32) negated;		verify: (2 raisedTo: 64);		verify: (2 raisedTo: 64) negated;		verify: 4598754392654025898794;		verify: -13750198234577893465! !

!RsrSpeciesReturnEquality methodsFor!
testSymbol	self		verify: #'';		verify: #symbol! !

!RsrSystemTestCase methodsFor!
tearDown	connectionA ifNotNil: [:conn | conn close].	connectionB ifNotNil: [:conn | conn close].	connectionA := connectionB := nil.	super tearDown! !

!RsrSystemTestCase methodsFor!
setUp	| socketPair semaphore |	super setUp.	socketPair := RsrSocketPair new.	connectionA := RsrConnection		socket: socketPair firstSocket		transactionSpigot: RsrThreadSafeNumericSpigot naturals		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1).	connectionB := RsrConnection		socket: socketPair secondSocket		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated		oidSpigot: RsrThreadSafeNumericSpigot naturals negated.	semaphore := Semaphore new.	self fork:		[[connectionA open] ensure: [semaphore signal]].	self fork:		[[connectionB open] ensure: [semaphore signal]].	semaphore wait; wait.	self		assert: connectionA isOpen;		assert: connectionB isOpen! !

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

!RsrServiceAbstractAllDataObjects methodsFor!
initialize	super initialize.	negativeSmallInteger := -1.	integerZero := 0.	positiveSmallInteger := 1.	string := 'hello, world'! !

!RsrAbstractTestService methodsFor!
sharedVariable: anObject	sharedVariable := anObject! !

!RsrAbstractTestService methodsFor!
remoteSelf	^remoteSelf! !

!RsrAbstractTestService methodsFor!
sharedVariable	^sharedVariable! !

!RsrPromiseTest methodsFor!
testFulfillment	| promise expected semaphore |	promise := RsrPromise new.	expected := Object new.	self fork:		[(Delay forSeconds: 1) wait.		promise fulfill: expected].	self		assert: promise value		identicalTo: expected.	promise := RsrPromise new.	semaphore := Semaphore new.	[promise fulfill: expected.	semaphore signal] fork.	semaphore wait.	self		assert: promise value		identicalTo: expected! !

!RsrPromiseTest methodsFor!
testError	| promise semaphore |	promise := RsrPromise new.	self fork:		[(Delay forSeconds: 1) wait.		promise error: Error new].	self		should: [promise value]		raise: Error.	promise := RsrPromise new.	semaphore := Semaphore new.	self fork:		[promise error: Error new.		semaphore signal].	self		should: [promise value]		raise: Error! !

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
forwarderClass: aClass	forwarderClass := aClass! !

!RsrLifetimeTest methodsFor!
testCloseWithDanglingObject	"It is possible that the connection could disconnect between when an object	is received and when the upcoming SendMessage or DeliverResponse message is received.	If this is the case, we could leak memory due to the caching used to ensure	the object is stored in memory long enough to process the upcoming message.	Test to ensure the object is freed on the connection close."	| service command |	self maximumReclamation.	self assert: RsrClientNoInstVars allInstances isEmpty.	service := RsrServerNoInstVars new.	service		_id: 2		connection: connectionA.	command := RsrRetainObject object: service.	command		encodeUsing: connectionA encoder;		writeUsing: connectionA commandWriter.	connectionA commandWriter flush.	(Delay forMilliseconds: 10) wait.	connectionA close.	connectionB close.	service := command := nil.	self maximumReclamation.	self assert: RsrClientNoInstVars allInstances isEmpty! !

!RsrLifetimeTest methodsFor!
testRemoteReferenceLifetime	| valueServiceLocal valueServiceRemote serviceLocal serviceRemote id marker actual |	serviceLocal := RsrClientNoInstVars new.	valueServiceLocal := connectionA serviceFor: #RsrValueHolderClient.	valueServiceLocal value: serviceLocal.	valueServiceRemote := connectionB registry at: valueServiceLocal _id.	serviceRemote := valueServiceRemote value.	id := serviceLocal _id.	self		assert: serviceRemote class		equals: RsrServerNoInstVars.	serviceLocal := serviceRemote := nil.	valueServiceRemote value: nil.	self maximumReclamation.	(Delay forSeconds: 1) wait. "Needed to ensure there is time for release to propogate to remote environment."	marker := Object new.	actual := connectionA registry at: id ifAbsent: [marker].	self		assert: actual		equals: marker.	actual := connectionB registry at: id ifAbsent: [marker].	self		assert: actual		equals: marker! !

!RsrLifetimeTest methodsFor!
testObjectCreatedViaServiceFactory	"Ensure objects created by the RsrServiceFactory maintain the same	properties as other objects"	| service remoteService |	service := connectionA serviceFor: #RsrClientNoInstVars.	self		assert: (connectionA registry at: service _id)		identicalTo: service.	remoteService := connectionB registry at: service _id.	self		assert: remoteService class		equals: RsrServerNoInstVars.	self		assert: service sendReturnSelf		identicalTo: service! !

!RsrClientNoInstVars methodsFor!
sendReturnTestSymbol	^remoteSelf returnTestSymbol! !

!RsrClientNoInstVars methodsFor!
sendSetMarker	^remoteSelf setMarker! !

!RsrClientNoInstVars methodsFor!
sendUnaryReturn42Message	^remoteSelf unaryReturn42! !

!RsrClientNoInstVars methodsFor!
sendSignalError	^remoteSelf signalError! !

!RsrClientNoInstVars methodsFor!
sendReturnNewServiceInArray	^remoteSelf returnNewServiceInArray! !

!RsrClientNoInstVars methodsFor!
sendReturnNewService	^remoteSelf returnNewService! !

!RsrClientNoInstVars methodsFor!
sendReturnObjectNew	^remoteSelf returnObjectNew! !

!RsrClientNoInstVars methodsFor!
sendReturnSelf	^remoteSelf returnSelf! !

!RsrDecoderTest methodsFor!
decodeService: anObjectBytes	^self decoder decodeService: anObjectBytes readStream! !

!RsrDecoderTest methodsFor!
testServiceDecodeIdentity	"Ensure that decoding an object multiple times results in	a single object getting created."	| firstService secondService |	firstService := self decodeService: self serviceNoInstVarsEncoding.	secondService := self decodeService: self serviceNoInstVarsEncoding.	self		assert: firstService		identicalTo: secondService! !

!RsrDecoderTest methodsFor!
testServiceReferenceService	| rootService referencedService |	referencedService := self decodeService: self referencedServiceEncoding.	self		assert: referencedService class		equals: RsrServerNoInstVars.	self		assert: referencedService _id		equals: 2.	rootService := self decodeService: self rootServiceEncoding.	self		assert: rootService class		equals: RsrServerReferenceService.	self		assert: rootService service		equals: referencedService! !

!RsrDecoderTest methodsFor!
testRetainObject	| encoding command |	encoding :=		#[0 0 0 0 0 0 0 0], "RetainObject Command"		#[0 0 0 0 0 0 0 0], "ServiceType Object"		#[0 0 0 0 0 0 0 1], "Service OID"		#[0 0 0 0 0 0 0 0], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 19], "Length of UTF-8 bytes"		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"	command := self decoder decodeCommand: encoding readStream.	self		assert: command object class		equals: RsrServerNoInstVars.	self		assert: command object _id		equals: 1.	self assert: (registry includesKey: command object _id)! !

!RsrDecoderTest methodsFor!
testDeliverResponse	| response encoding command |	response := #responseSymbol.	encoding :=		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"		#[0 0 0 0 0 0 0 1], "Transaction Id"		#[0 0 0 0 0 0 0 0], "nil errorName"		#[0 0 0 0 0 0 0 6], "nil errorName"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 14], "Length of UTF-8 bytes"		#[114 101 115 112 111 110 115 101 83 121 109 98 111 108]. "#responseSymbol"	command := self decoder decodeCommand: encoding readStream.	self		assert: command class		equals: RsrDeliverResponse.	self		assert: command transaction		equals: 1.	self		assert: command response		equals: response! !

!RsrDecoderTest methodsFor!
verifyImmediate: expectedencoding: encoding	| actual |	actual := self decoder decodeObjectReference: encoding readStream.	self		assert: actual		equals: expected! !

!RsrDecoderTest methodsFor!
testServiceNoInstVars	| decodedService |	decodedService := self decodeService: self serviceNoInstVarsEncoding.	self		assert: decodedService class		equals: RsrServerNoInstVars.	self		assert: decodedService _id		equals: 1! !

!RsrDecoderTest methodsFor!
assertReference: bytesdecodesTo: expected	| actual |	actual := self decoder decodeObjectReference: bytes readStream.	self		assert: actual		equals: expected! !

!RsrDecoderTest methodsFor!
verifyControlWord: expectedencoding: bytes	| actual |	actual := self decoder decodeControlWord: bytes readStream.	self		assert: actual		equals: expected! !

!RsrDecoderTest methodsFor!
testSendMessage	| service encoding command |	service := RsrServerNoInstVars		_id: 1		connection: RsrMockConnection new.	self decoder.	service _addTo: registry.	encoding :=		#[0 0 0 0 0 0 0 1], "SendMessage Command"		#[0 0 0 0 0 0 0 1], "Transaction ID"		#[0 0 0 0 0 0 0 0], "Argument Count"		#[0 0 0 0 0 0 0 1], "Receiver OID"		#[0 0 0 0 0 0 0 0], "Selector Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 8], "Length of UTF-8 bytes"		#[114 101 116 117 114 110 52 50]. "#return42"	command := self decoder decodeCommand: encoding readStream.	self		assert: command class		equals: RsrSendMessage.	self		assert: command transaction		equals: 1.	self		assert: command receiver		identicalTo: service.	self		assert: command selector		identicalTo: #return42.	self		assert: command arguments		equals: #()! !

!RsrDecoderTest methodsFor!
testReleaseObjects	| command encoding |	encoding :=		#[0 0 0 0 0 0 0 3], "ReleaseObjects Command"		#[0 0 0 0 0 0 0 5], "Num OIDS"		#[0 0 0 0 0 0 0 1], "First OID"		#[0 0 0 0 0 0 0 2],		#[0 0 0 0 0 0 0 3],		#[0 0 0 0 0 0 0 4],		#[0 0 0 0 0 0 0 5]. "Last OID"	command := self decoder decodeCommand: encoding readStream.	self		assert: command oids		equals: #(1 2 3 4 5)! !

!RsrForwarderTest methodsFor!
testForwarding	| rsrObject id remoteInterface forwarder message |	rsrObject := RsrClientTestService new.	id := 1.	remoteInterface := RsrMockConnection forwarderClass: RsrForwarder.	rsrObject		_id: id		connection: remoteInterface.	forwarder := rsrObject remoteSelf.	forwarder		arg1: 15		arg2: 42.	message := remoteInterface lastMessage.	self		assert: message transaction		equals: 1.	self		assert: message receiver		equals: rsrObject.	self		assert: message selector		equals: #arg1:arg2:.	self		assert: message arguments		equals: #(15 42).! !

!RsrEncoderTest methodsFor!
setUp	super setUp.	registry := RsrMockRegistry new.	connection := RsrMockConnection new! !

!RsrEncoderTest methodsFor!
tearDown	registry := connection := nil.	super setUp! !

!RsrEncoderTest methodsFor!
testUnsupportedObject	self		should: [self encoder encodeObject: Object new]		raise: RsrUnsupportedObject.	self		should: [self encoder encodeReferenceOf: Object new onto: (WriteStream on: ByteArray new)]		raise: RsrUnsupportedObject! !

!RsrEncoderTest methodsFor!
testRetainObject	| service command expectedEncoding |	service := RsrClientNoInstVars new.	self register: service.	command := RsrRetainObject object: service.	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 0], "RetainObject Command"		#[0 0 0 0 0 0 0 0], "ServiceType Object"		#[0 0 0 0 0 0 0 1], "Service OID"		#[0 0 0 0 0 0 0 0], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 19], "Length of UTF-8 bytes"		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"	self		assert: command encoding		equals: expectedEncoding! !

!RsrEncoderTest methodsFor!
testServiceReferenceService	| rootService referencedService encodedObject expectedEncoding |	referencedService := RsrClientNoInstVars new.	rootService := RsrClientReferenceService service: referencedService.	self		register: rootService;		register: referencedService.	encodedObject := self encoder encodeObject: rootService.	expectedEncoding :=		#[0 0 0 0 0 0 0 0], "type"		#[0 0 0 0 0 0 0 1], "rootService's OID = 1"		#[0 0 0 0 0 0 0 1], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"		#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"		#[0 0 0 0 0 0 0 25], "Length of UTF-8 encoded bytes"		#[82 115 114 83 101 114 118 101 114 82 101 102 101 114 101 110 99 101 83 101 114 118 105 99 101],		#[0 0 0 0 0 0 0 2]. "#RsrServerReferenceService"	self		assert: encodedObject		equals: expectedEncoding.	encodedObject := self encoder encodeObject: referencedService.	expectedEncoding :=		#[0 0 0 0 0 0 0 0], "type"		#[0 0 0 0 0 0 0 2], "referencedService's OID = 2"		#[0 0 0 0 0 0 0 0], "Inst Var Count"		#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"		#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"		#[0 0 0 0 0 0 0 19], "Length of UTF-8 encoded bytes"		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"	self		assert: encodedObject		equals: expectedEncoding! !

!RsrEncoderTest methodsFor!
testDeliverResponse	| response command expectedEncoding |	response := #responseSymbol.	command := RsrDeliverResponse		transaction: 1		response: response.	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"		#[0 0 0 0 0 0 0 1], "Transaction Id"		#[0 0 0 0 0 0 0 0], "nil errorName"		#[0 0 0 0 0 0 0 6], "nil errorName"		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"		#[0 0 0 0 0 0 0 14], "Length of UTF-8 bytes"		#[114 101 115 112 111 110 115 101 83 121 109 98 111 108]. "#responseSymbol"	self		assert: command encoding		equals: expectedEncoding! !

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
testReleaseObjects	| command expectedEncoding |	command := RsrReleaseObjects oids: #(1 2 3 4 5).	command encodeUsing: RsrEncoder new.	expectedEncoding :=		#[0 0 0 0 0 0 0 3], "ReleaseObjects Command"		#[0 0 0 0 0 0 0 5], "Num OIDS"		#[0 0 0 0 0 0 0 1], "First OID"		#[0 0 0 0 0 0 0 2],		#[0 0 0 0 0 0 0 3],		#[0 0 0 0 0 0 0 4],		#[0 0 0 0 0 0 0 5]. "Last OID"	self		assert: command encoding		equals: expectedEncoding! !

!RsrSocketStreamTestCase methodsFor!
tearDown	serverStream close.	clientStream close.	super tearDown! !

!RsrSocketStreamTestCase methodsFor!
setUp	super setUp.	self initializeStreams! !

!RsrSocketStreamTestCase methodsFor!
initializeStreams	| listener client server |	listener := RsrSocket new.	client := RsrSocket new.	listener listenOn: self listenPort.	client connectTo: self listenPort on: '127.0.0.1'.	server := listener accept.	listener close.	serverStream := RsrSocketStream on: server.	clientStream := RsrSocketStream on: client! !

!RsrSocketStreamTestCase methodsFor!
testClose	serverStream close.	self		deny: serverStream isConnected;		assert: clientStream isConnected.	self		should: [clientStream next]		raise: RsrSocketClosed.	self		deny: clientStream isConnected! !

!RsrSocketStreamTestCase methodsFor!
listenPort	^47856! !