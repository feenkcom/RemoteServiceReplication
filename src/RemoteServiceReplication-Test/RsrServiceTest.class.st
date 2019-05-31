Class {
	#name : #RsrServiceTest,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
RsrServiceTest >> testInitialization [

	| service |
	service := RsrClientTestService new.
	self
		assertObject: service
		isMirrored: false
		id: nil
		connection: nil.
]

{ #category : #running }
RsrServiceTest >> testMirrored [

	| service id connection |
	service := RsrClientTestService new.
	id := self newId.
	connection := self newTestConnection.
	service
		rsrId: id;
		rsrConnection: connection.
	self
		assertObject: service
		isMirrored: true
		id: id
		connection: connection.
	self
		assert: service remoteSelf class
		equals: RsrMockForwarder
]
