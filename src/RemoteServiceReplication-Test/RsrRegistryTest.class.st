"
I represent tests for the RsrRegistry.
"
Class {
	#name : #RsrRegistryTest,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #tests }
RsrRegistryTest >> testRegister [

	| id object registry marker |
	id := 1.
	marker := Object new.
	object := RsrClientNoInstVars
		rsrId: id
		rsrConnection: RsrMockConnection new.
	registry := RsrRegistry new.
	registry register: object.
	self maximumReclamation.
	self
		assert: (registry at: id ifAbsent: [marker])
		identicalTo: object.
	object := nil.
	self maximumReclamation.
	self
		assert: (registry at: id ifAbsent: [marker])
		identicalTo: marker
]

{ #category : #tests }
RsrRegistryTest >> testRetain [

	| id object registry marker |
	id := 1.
	marker := Object new.
	object := RsrServerNoInstVars
		rsrId: id
		rsrConnection: RsrMockConnection new.
	registry := RsrRegistry new.
	registry retain: object.
	object := nil.
	self maximumReclamation.
	object := registry at: id ifAbsent: [marker].
	self
		deny: object
		equals: marker.
	self
		assert: object class
		equals: RsrServerNoInstVars.
	self
		assert: object rsrId
		equals: id
]
