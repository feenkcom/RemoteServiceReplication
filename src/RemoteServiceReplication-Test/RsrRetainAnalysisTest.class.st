Class {
	#name : 'RsrRetainAnalysisTest',
	#superclass : 'RsrTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'utilities' }
RsrRetainAnalysisTest >> analyze: anObject [

	| analysis |
	analysis := RsrRetainAnalysis
		roots: (Array with: anObject)
		connection: RsrMockConnection new.
	analysis perform.
	^analysis
]

{ #category : 'utilities' }
RsrRetainAnalysisTest >> assertCycle: anObject [

	self
		should: [self analyze: anObject]
		raise: RsrCycleDetected
]

{ #category : 'running' }
RsrRetainAnalysisTest >> testArrayCycle [

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
RsrRetainAnalysisTest >> testDictionaryCycle [

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
RsrRetainAnalysisTest >> testNewServiceInArray [
	"Ensure a new service in a collection is properly tagged"

	| service analysis expected |
	service := RsrServerNoInstVars new.
	analysis := self analyze: (Array with: service).
	expected := OrderedCollection
		with: (RsrRetainObject object: service encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self assert: service isMirrored
]

{ #category : 'running' }
RsrRetainAnalysisTest >> testNewServicesInDictionary [
	"Ensure a new service in a collection is properly tagged"

	| key value dictionary analysis expected |
	key := RsrServerNoInstVars new.
	value := RsrServerNoInstVars new.
	dictionary := Dictionary new
		at: key put: value;
		yourself.
	analysis := self analyze: dictionary.
	expected := OrderedCollection
		with: (RsrRetainObject object: key encoding: ByteArray new)
		with: (RsrRetainObject object: value encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self
		assert: key isMirrored;
		assert: value isMirrored
]

{ #category : 'running' }
RsrRetainAnalysisTest >> testOrderedCollectionCycle [

	| oc |
	oc := OrderedCollection new.
	oc add: oc.
	self assertCycle: oc.
	oc := OrderedCollection with: (Array with: oc).
	self assertCycle: oc.
]

{ #category : 'running' }
RsrRetainAnalysisTest >> testServiceAllDataObjects [
	"While this code is structurally similar to #testClientNoInstVars, it ensures
	that Data Objects are actually encoded in-line."

	| client registry analysis expected |
	client := RsrRemoteAction clientClass new.
	registry := RsrMockRegistry new.
	analysis := self analyze: client.
	expected := OrderedCollection
		with: (RsrRetainObject object: client encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self assert: client isMirrored
]

{ #category : 'running' }
RsrRetainAnalysisTest >> testServiceNoInstVars [

	| client analysis expected |
	client := RsrClientNoInstVars new.
	analysis := self analyze: client.
	expected := OrderedCollection
		with: (RsrRetainObject object: client encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self assert: client isMirrored
]

{ #category : 'running' }
RsrRetainAnalysisTest >> testServiceReferencingAnotherService [
	"While this code is structurally similar to #testClientNoInstVars, it ensures
	that Data Objects are actually encoded in-line."

	| referencedService client analysis expected |
	referencedService := RsrRemoteAction clientClass new.
	client := RsrRemoteAction clientClass sharedVariable: referencedService.
	analysis := self analyze: client.
	expected := OrderedCollection
		with: (RsrRetainObject object: referencedService encoding: ByteArray new)
		with: (RsrRetainObject object: client encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self
		assert: client isMirrored;
		assert: referencedService isMirrored
]

{ #category : 'running' }
RsrRetainAnalysisTest >> testServiceWithCycle [
	"Cycles are disallowed for our POC. Perhaps they will get added later?"

	| rootClient referencedClient |
	rootClient := RsrRemoteAction new.
	referencedClient := RsrRemoteAction sharedVariable: rootClient.
	rootClient sharedVariable: referencedClient.
	self assertCycle: rootClient
]

{ #category : 'running' }
RsrRetainAnalysisTest >> testSetCycle [

	| set |
	set := Set new.
	set add: set.
	self assertCycle: set.
	set := Set new.
	set add: (Array with: set).
	self assertCycle: set
]
