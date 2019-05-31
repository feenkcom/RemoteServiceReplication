"
An abstract test class which contains utility methods
"
Class {
	#name : #RsrTestCase,
	#superclass : #TestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
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
RsrTestCase >> maximumReclamation [

	| obj sema |
	obj := Object new.
	sema := Semaphore new.
	obj
		toFinalizeSend: #cull:
		to: [sema signal]
		with: nil.
	obj := nil.
	Smalltalk garbageCollect.
	Processor yield.
	self deny: (sema waitTimeoutMSecs: 100).
	
]
