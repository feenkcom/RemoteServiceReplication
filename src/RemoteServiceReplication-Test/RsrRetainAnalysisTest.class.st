Class {
	#name : #RsrRetainAnalysisTest,
	#superclass : #RsrTestCase,
	#category : 'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
RsrRetainAnalysisTest >> testArray [

	| service array registry analysis expected |
	service := RsrClientNoInstVars new.
	array := (RsrArray new: 1)
		at: 1 put: service;
		yourself.
	registry := RsrMockRegistry new.
	analysis := RsrRetainAnalysis
		roots: (Array with: array)
		connection: RsrMockConnection new.
	analysis perform.
	expected := OrderedCollection
		with: (RsrRetainObject object: service encoding: ByteArray new)
		with: (RsrRetainObject object: array encoding: ByteArray new).
	self
		assert: analysis retainCommands
		equals: expected.
	self
		assert: service isMirrored;
		assert: array isMirrored
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
