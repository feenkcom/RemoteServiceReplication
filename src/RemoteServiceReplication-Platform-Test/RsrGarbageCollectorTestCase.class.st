Class {
	#name : 'RsrGarbageCollectorTestCase',
	#superclass : 'RsrTestCase',
	#category : 'RemoteServiceReplication-Platform-Test'
}

{ #category : 'running' }
RsrGarbageCollectorTestCase >> testMaximumReclamation [

	self assert: RsrGarbageCollector maximumReclamation
]
