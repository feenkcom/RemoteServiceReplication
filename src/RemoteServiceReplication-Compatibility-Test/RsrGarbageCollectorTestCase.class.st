Class {
	#name : #RsrGarbageCollectorTestCase,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Compatibility-Test-TestCases'
}

{ #category : #running }
RsrGarbageCollectorTestCase >> testMaximumReclamation [

	self assert: RsrGarbageCollector maximumReclamation
]
