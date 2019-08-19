"
I represent tests for the RsrRegistry.
"
Class {
	#name : #RsrRegistryTestCase,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Compatibility-Test'
}

{ #category : #tests }
RsrRegistryTestCase >> testRegister [

	| id object registry marker |
	marker := Object new.
	object := RsrMockService new.
	id := object rsrId.
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
RsrRegistryTestCase >> testRetain [

	| id object registry marker |
	marker := Object new.
	object := RsrMockService new.
	id := object rsrId.
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
		equals: RsrMockService.
	self
		assert: object rsrId
		equals: id
]
