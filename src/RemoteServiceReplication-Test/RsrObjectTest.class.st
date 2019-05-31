"
I represent tests for the RsrObjects
"
Class {
	#name : #RsrObjectTest,
	#superclass : #RsrTestCase,
	#category : 'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
RsrObjectTest >> testInitialization [

	| localObject |
	localObject := RsrObject new.
	self
		assertObject: localObject
		isMirrored: false
		id: nil
		connection: nil
]

{ #category : #running }
RsrObjectTest >> testMirrored [

	| anObject id connection |
	anObject := RsrObject new.
	id := self newId.
	connection := self newTestConnection.
	anObject
		rsrId: id;
		rsrConnection: connection.
	self
		assertObject: anObject
		isMirrored: true
		id: id
		connection: connection
]
