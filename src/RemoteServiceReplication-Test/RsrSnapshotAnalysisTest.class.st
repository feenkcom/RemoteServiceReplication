"
No class-specific documentation for RsrSnapshotAnalysisTest, hierarchy is:
Object
  TestAsserter
    TestCase( testSelector)
      RsrTestCase
        RsrSnapshotAnalysisTest

"
Class {
	#name : 'RsrSnapshotAnalysisTest',
	#superclass : 'RsrTestCase',
	#instVars : [
		'connection'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'utilites' }
RsrSnapshotAnalysisTest >> analyze: anObject [

	| analysis |
	analysis := RsrSnapshotAnalysis
		roots: (Array with: anObject)
		connection: connection.
	analysis perform.
	^analysis
]

{ #category : 'utilites' }
RsrSnapshotAnalysisTest >> assertCycle: anObject [

	self
		should: [self analyze: anObject]
		raise: RsrCycleDetected
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> setUp [

	super setUp.
	connection := RsrConnection
		channel: RsrNullChannel new
		transactionSpigot: RsrThreadSafeNumericSpigot naturals
		oidSpigot: RsrThreadSafeNumericSpigot naturals.
	connection open
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> tearDown [

	connection close.
	connection := nil.
	super tearDown
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> testArrayCycle [

	| array |
	array := Array new: 1.
	array
		at: 1
		put: array.
	self assertCycle: array.
	array
		at: 1
		put: { array }.
	self assertCycle: array
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> testDictionaryCycle [

	| dictionary |
	dictionary := Dictionary new.
	dictionary
		at: 1
		put: dictionary.
	self assertCycle: dictionary.
	dictionary removeKey: 1.
	dictionary
		at: dictionary
		put: 1.
	self assertCycle: dictionary
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> testNewServiceInArray [
	"Ensure a new service in a collection is properly tagged"

	| service analysis expected |
	service := RsrServerNoInstVars new.
	analysis := self analyze: (Array with: service).
	expected := OrderedCollection with: service.
	self
		assert: analysis snapshots size
		equals: 1.
	self assert: service isMirrored
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> testNewServicesInDictionary [
	"Ensure a new service in a collection is properly tagged"

	| key value dictionary analysis expected |
	key := RsrServerNoInstVars new.
	value := RsrServerNoInstVars new.
	dictionary := Dictionary new
		at: key put: value;
		yourself.
	analysis := self analyze: dictionary.
	self
		assert: analysis snapshots size
		equals: 2.
	self
		assert: key isMirrored;
		assert: value isMirrored
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> testOrderedCollectionCycle [

	| oc |
	oc := OrderedCollection new.
	oc add: oc.
	self assertCycle: oc.
	oc := OrderedCollection with: (Array with: oc).
	self assertCycle: oc.
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> testServiceAllDataObjects [
	"While this code is structurally similar to #testClientNoInstVars, it ensures
	that Data Objects are actually encoded in-line."

	| client analysis expected |
	client := RsrRemoteAction clientClass new.
	analysis := self analyze: client.
	expected := OrderedCollection with: client.
	self
		assert: analysis snapshots size
		equals: 1.
	self assert: client isMirrored
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> testServiceNoInstVars [

	| client analysis expected snapshot |
	client := RsrClientNoInstVars new.
	analysis := self analyze: client.
	expected := OrderedCollection with: client.
	self assert: client isMirrored.
	self
		assert: analysis snapshots size
		equals: 1.
	snapshot := analysis snapshots first.
	self
		assert: snapshot slots size
		equals: 0.
	self assert: snapshot shouldCreateServer.
	self
		assert: snapshot templateClass
		equals: client class templateClass
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> testServiceReferencingAnotherService [
	"While this code is structurally similar to #testClientNoInstVars, it ensures
	that Data Objects are actually encoded in-line."

	| referencedService client analysis expected |
	referencedService := RsrRemoteAction clientClass new.
	client := RsrRemoteAction clientClass sharedVariable: referencedService.
	analysis := self analyze: client.
	self
		assert: analysis snapshots size
		equals: 2.
	self
		assert: client isMirrored;
		assert: referencedService isMirrored
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> testServiceWithCycle [
	"Cycles are disallowed for our POC. Perhaps they will get added later?"

	| rootClient referencedClient |
	rootClient := RsrRemoteAction new.
	referencedClient := RsrRemoteAction sharedVariable: rootClient.
	rootClient sharedVariable: referencedClient.
	self assertCycle: rootClient
]

{ #category : 'running' }
RsrSnapshotAnalysisTest >> testSetCycle [

	| set |
	set := Set new.
	set add: set.
	self assertCycle: set.
	set := Set new.
	set add: (Array with: set).
	self assertCycle: set
]
