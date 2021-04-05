Class {
	#name : 'RsrEphemeronMourningDeadlock',
	#superclass : 'RsrSystemTestCase',
	#category : 'RemoteServiceReplication-GemStone-Test'
}

{ #category : 'running' }
RsrEphemeronMourningDeadlock >> setUp [

	super setUp.
	self initializeSocketConnections
]

{ #category : 'running' }
RsrEphemeronMourningDeadlock >> testEphemeronMourningDoesNotDeadlock [
	"The RsrRegistry protects its backing datastructure using a mutex.
	When returning a large number of new Services, the data structure
	can grow triggering a garbage collection. Any services which are 
	collected need to be removed from the registry.
	The ephemeron mourning by default happens on the currently active
	thread. This can result in attempting to access the critical section in
	the registry twice. It isn't safe to use a recursive lock.
	This test is intended to recreate this case ensuring that RSR properly
	activates a background mourning process preventing the deadlock.

	We lock the registry by waiting in a critical section. Then, we mourn an ephemeron.
	This should force the mourning process to wait until the test exits the critical section."

	| service semaphore isRunning |
	"Ensure we are processing #mourn async"
	self assert: RsrAsyncMournHandler current isActive.
	"We now have an ephemeron setup for this service."
	service := RsrClientNoInstVars new
		registerWith: connectionA;
		yourself.
	semaphore := Semaphore new.
	isRunning := true.
	RsrProcessModel
		fork:
			[connectionA _privateRegistryOnlyForTests critical: [semaphore signal; wait]]
		named: 'Lock registry'.
	[semaphore wait.
	service := nil.
	RsrGarbageCollector maximumReclamation.
	self
		assert: RsrAsyncMournHandler current process waitingOn class
		equals: Semaphore]
		ensure: [semaphore signal]
]
