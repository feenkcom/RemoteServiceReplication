Class {
	#name : #RsrServiceRegistryTest,
	#superclass : #RsrSystemTestCase,
	#category : 'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
RsrServiceRegistryTest >> testCloseWithDanglingObjectInRegistry [
	"Since the Connection is used in finalization, it cannot hold a strong reference
	to objects that should be finalized. The ServiceRegistry may hold onto such a reference.
	Ensure objects stored in the ServiceRegistry do not prevent finalization."

	| name service marker actual |
	name := 'TestService'.
	service := RsrClientNoInstVars new.
	marker := Object new.
	connectionA serviceRegistry
		at: name
		put: service.
	actual := connectionA serviceRegistry at: name.
	self
		assert: actual
		identicalTo: actual.
	actual := connectionB serviceRegistry at: name.
	self
		assert: actual class
		equals: RsrServerNoInstVars.
	self
		assert: actual rsrId
		equals: service rsrId
]

{ #category : #running }
RsrServiceRegistryTest >> testRegistration [

	| name service marker actual |
	name := 'TestService'.
	service := RsrClientNoInstVars new.
	marker := Object new.
	connectionA serviceRegistry
		at: name
		put: service.
	actual := connectionA serviceRegistry at: name.
	self
		assert: actual
		identicalTo: actual.
	actual := connectionB serviceRegistry at: name.
	self
		assert: actual class
		equals: RsrServerNoInstVars.
	self
		assert: actual rsrId
		equals: service rsrId.
	connectionB serviceRegistry removeKey: name.
	self
		assert: (connectionA serviceRegistry at: name ifAbsent:[marker])
		identicalTo: marker.
	self
		assert: (connectionB serviceRegistry at: name ifAbsent:[marker])
		identicalTo: marker
]
