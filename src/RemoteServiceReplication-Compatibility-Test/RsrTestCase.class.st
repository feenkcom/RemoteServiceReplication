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

{ #category : #accessing }
RsrTestCase >> defaultTimeLimit [

	^5 seconds
]

{ #category : #utilities }
RsrTestCase >> hack: aString [
	"Placeholder for things that need to be fixed"
]

{ #category : #utilities }
RsrTestCase >> maximumReclamation [

	self assert: RsrGarbageCollector maximumReclamation
]
