"
I represent tests for the RsrRegistry.
"
Class {
	#name : 'RsrRegistryTestCase',
	#superclass : 'RsrTestCase',
	#instVars : [
		'registry'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'running' }
RsrRegistryTestCase >> setUp [

	super setUp.
	registry := RsrRegistry new
]

{ #category : 'running' }
RsrRegistryTestCase >> tearDown [

	registry := nil.
	super tearDown
]

{ #category : 'tests' }
RsrRegistryTestCase >> testAddClient [

	| id object entry marker |
	marker := Object new.
	object := RsrMockClient new.
	id := object _id.
	registry
		serviceAt: id
		put: object.
	self maximumReclamation.
	self
		assert: (registry serviceAt: id ifAbsent: [marker])
		identicalTo: object.
	object := nil.
	self maximumReclamation.
	self
		assert: (registry serviceAt: id ifAbsent: [marker])
		identicalTo: marker
]

{ #category : 'tests' }
RsrRegistryTestCase >> testAddServer [

	| id object marker |
	marker := Object new.
	object := RsrMockServer new.
	id := object _id.
	registry
		serviceAt: id
		put: object.
	object := nil.
	self maximumReclamation.
	object := registry serviceAt: id ifAbsent: [marker].
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

{ #category : 'tests' }
RsrRegistryTestCase >> testAtAtIfAbsent [

	| server id marker |
	server := RsrMockServer new.
	id := server _id.
	self
		should: [registry serviceAt: id]
		raise: Error.
	marker := Object new.
	self
		assert: (registry serviceAt: id ifAbsent: [marker])
		identicalTo: marker.
	registry
		serviceAt: id
		put: server.
	self
		assert: (registry serviceAt: id)
		identicalTo: server.
	self
		assert: (registry serviceAt: id ifAbsent: [marker])
		identicalTo: server
]

{ #category : 'tests' }
RsrRegistryTestCase >> testIncludesKey [

	| client |
	client := RsrMockClient new.
	self deny: (registry includesKey: client _id).
	registry
		serviceAt: client _id
		put: client.
	self assert: (registry includesKey: client _id)
]

{ #category : 'tests' }
RsrRegistryTestCase >> testRemoveKey [

	| client |
	client := RsrMockClient new.
	self
		assert: (registry removeKey: client _id)
		equals: nil.
	registry
		serviceAt: client _id
		put: client.
	self
		assert: (registry removeKey: client _id) service
		identicalTo: client
]
