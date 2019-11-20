Class {
	#name : #RsrRetainAnalysisTest,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
RsrRetainAnalysisTest >> analyze: anObject [

	| analysis |
	analysis := RsrRetainAnalysis
		roots: (Array with: anObject)
		connection: RsrMockConnection new.
	analysis perform
]

{ #category : #running }
RsrRetainAnalysisTest >> testArrayCycle [

	| array |
	array := Array new: 1.
	array
		at: 1
		put: array.
	self
		should: [self analyze: array]
		raise: RsrCycleDetected.
	array
		at: 1
		put: { array }.
	self
		should: [self analyze: array]
		raise: RsrCycleDetected
]

{ #category : #running }
RsrRetainAnalysisTest >> testDictionaryCycle [

	| dictionary |
	dictionary := Dictionary new.
	dictionary
		at: 1
		put: dictionary.
	self
		should: [self analyze: dictionary]
		raise: RsrCycleDetected.
	dictionary removeKey: 1.
	dictionary
		at: dictionary
		put: 1.
	self
		should: [self analyze: dictionary]
		raise: RsrCycleDetected
]

{ #category : #running }
RsrRetainAnalysisTest >> testNewServiceInArray [
	"Ensure a new service in a collection is properly tagged"

	| service analysis expected |
	service := RsrServerNoInstVars new.
	analysis := RsrRetainAnalysis
		roots: (Array with: (Array with: service))
		connection: RsrMockConnection new.
	analysis perform.
	expected := OrderedCollection
		with: (RsrRetainObject object: service encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self assert: service isMirrored
]

{ #category : #running }
RsrRetainAnalysisTest >> testNewServicesInDictionary [
	"Ensure a new service in a collection is properly tagged"

	| key value dictionary analysis expected |
	key := RsrServerNoInstVars new.
	value := RsrServerNoInstVars new.
	dictionary := Dictionary new
		at: key put: value;
		yourself.
	analysis := RsrRetainAnalysis
		roots: (Array with: dictionary)
		connection: RsrMockConnection new.
	analysis perform.
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

{ #category : #running }
RsrRetainAnalysisTest >> testOrderedCollectionCycle [

	| oc |
	oc := OrderedCollection new.
	oc add: oc.
	self
		should: [self analyze: oc]
		raise: RsrCycleDetected.
	oc := OrderedCollection with: (Array with: oc).
	self
		should: [self analyze: oc]
		raise: RsrCycleDetected
]

{ #category : #running }
RsrRetainAnalysisTest >> testServiceAllDataObjects [
	"While this code is structurally similar to #testClientNoInstVars, it ensures
	that Data Objects are actually encoded in-line."

	| client registry analysis expected |
	client := RsrClientAllDataObjects new.
	registry := RsrMockRegistry new.
	analysis := RsrRetainAnalysis
		roots: (Array with: client)
		connection: RsrMockConnection new.
	analysis perform.
	expected := OrderedCollection
		with: (RsrRetainObject object: client encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self assert: client isMirrored
]

{ #category : #running }
RsrRetainAnalysisTest >> testServiceNoInstVars [

	| client registry analysis expected |
	client := RsrClientNoInstVars new.
	registry := RsrMockRegistry new.
	analysis := RsrRetainAnalysis
		roots: (Array with: client)
		connection: RsrMockConnection new.
	analysis perform.
	expected := OrderedCollection
		with: (RsrRetainObject object: client encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self assert: client isMirrored
]

{ #category : #running }
RsrRetainAnalysisTest >> testServiceReferencingAnotherService [
	"While this code is structurally similar to #testClientNoInstVars, it ensures
	that Data Objects are actually encoded in-line."

	| referencedService client registry analysis expected |
	referencedService := RsrClientNoInstVars new.
	client := RsrClientReferenceService service: referencedService.
	registry := RsrMockRegistry new.
	analysis := RsrRetainAnalysis
		roots: (Array with: client)
		connection: RsrMockConnection new.
	analysis perform.
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

{ #category : #running }
RsrRetainAnalysisTest >> testServiceWithCycle [
	"Cycles are disallowed for our POC. Perhaps they will get added later?"

	| rootClient referencedClient registry analysis |
	rootClient := RsrClientReferenceService new.
	referencedClient := RsrClientReferenceService service: rootClient.
	rootClient service: referencedClient.
	registry := RsrMockRegistry new.
	analysis := RsrRetainAnalysis
		roots: (Array with: rootClient)
		connection: RsrMockConnection new.
	self
		should: [analysis perform]
		raise: RsrCycleDetected
]

{ #category : #running }
RsrRetainAnalysisTest >> testSetCycle [

	| set |
	set := Set new.
	set add: set.
	self
		should: [self analyze: set]
		raise: RsrCycleDetected.
	set := Set new.
	set add: (Array with: set).
	self
		should: [self analyze: set]
		raise: RsrCycleDetected
]
