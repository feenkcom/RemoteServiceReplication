Class {
	#name : #RsrGarbageCollectorTestCase,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Compatibility-Test'
}

{ #category : #running }
RsrGarbageCollectorTestCase >> testMaximumReclamation [

	self assert: RsrGarbageCollector maximumReclamation
]
