"
An abstract test class which contains utility methods
"
Class {
	#name : #RsrTestCase,
	#superclass : #TestCase,
	#category : #'RemoteServiceReplication-Compatibility-Test'
}

{ #category : #testing }
RsrTestCase class >> isAbstract [

	^self == RsrTestCase
]

{ #category : #asserting }
RsrTestCase >> assert: anObject
identicalTo: bObject [

	self assert: anObject == bObject
]

{ #category : #accessing }
RsrTestCase >> defaultTimeLimit [

	^5 seconds
]

{ #category : #asserting }
RsrTestCase >> deny: anObject
identicalTo: bObject [

	self assert: anObject ~~ bObject
]

{ #category : #utilities }
RsrTestCase >> fork: aBlock [

	^RsrConcurrency fork: aBlock
]

{ #category : #utilities }
RsrTestCase >> hack: aString [
	"Placeholder for things that need to be fixed"
]

{ #category : #utilities }
RsrTestCase >> maximumReclamation [

	self assert: RsrGarbageCollector maximumReclamation
]

{ #category : #running }
RsrTestCase >> runCase [

	| concurrency |
	concurrency := RsrTestingConcurrency new.
	RsrConcurrency current: concurrency.
	super runCase.
	RsrConcurrency resetCurrent.
	concurrency forkedException ifNotNil: [:ex | ex signal]
]
