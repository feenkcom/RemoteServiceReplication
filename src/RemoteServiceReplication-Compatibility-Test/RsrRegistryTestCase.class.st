"
I represent tests for the RsrRegistry.
"
Class {
	#name : #RsrRegistryTestCase,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Compatibility-Test'
}

{ #category : #tests }
RsrRegistryTestCase >> testAddClient [

	| id object registry marker |
	marker := Object new.
	object := RsrMockClient new.
	id := object _id.
	registry := RsrRegistry new.
	registry
		at: id
		put: object.
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
RsrRegistryTestCase >> testAddServer [

	| id object registry marker |
	marker := Object new.
	object := RsrMockServer new.
	id := object _id.
	registry := RsrRegistry new.
	registry
		at: id
		put: object.
	object := nil.
	self maximumReclamation.
	object := registry at: id ifAbsent: [marker].
	self
		deny: object
		equals: marker.
	self
		assert: object class
		equals: RsrMockServer.
	self
		assert: object _id
		equals: id
]
