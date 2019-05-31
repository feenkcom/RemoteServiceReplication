Class {
	#name : #RsrServiceRegistryTest,
	#superclass : #RsrSystemTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
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
	self sharedNamespaceA
		at: name
		put: service.
	actual := self sharedNamespaceA at: name.
	self
		assert: actual
		identicalTo: actual.
	actual := self sharedNamespaceB at: name.
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
	self sharedNamespaceA
		at: name
		put: service.
	actual := self sharedNamespaceA at: name.
	self
		assert: actual
		identicalTo: actual.
	actual := self sharedNamespaceB at: name.
	self
		assert: actual class
		equals: RsrServerNoInstVars.
	self
		assert: actual rsrId
		equals: service rsrId.
	self sharedNamespaceB removeKey: name.
	self
		assert: (self sharedNamespaceA at: name ifAbsent:[marker])
		identicalTo: marker.
	self
		assert: (self sharedNamespaceB at: name ifAbsent:[marker])
		identicalTo: marker
]
