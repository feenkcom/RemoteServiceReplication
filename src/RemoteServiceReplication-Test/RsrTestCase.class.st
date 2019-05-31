"
An abstract test class which contains utility methods
"
Class {
	#name : #RsrTestCase,
	#superclass : #TestCase,
	#category : 'RemoteServiceReplication-Test-TestCases'
}

{ #category : #testing }
RsrTestCase class >> isAbstract [

	^self == RsrTestCase
]

{ #category : #asserting }
RsrTestCase >> assertObject: anObject
isMirrored: isMirrored
id: id
connection: remoteInterface [

	self
		assert: anObject
		equals: anObject.
	self
		assert: anObject isMirrored
		equals: isMirrored.
	self
		assert: anObject rsrId
		equals: id.
	self
		assert: anObject rsrConnection
		equals: remoteInterface
]

{ #category : #accessing }
RsrTestCase >> defaultTimeLimit [

	^5 seconds
]

{ #category : #asserting }
RsrTestCase >> hack: aString [

	Transcript
		show: aString;
		cr
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

{ #category : #accessing }
RsrTestCase >> newId [

	^1
]

{ #category : #accessing }
RsrTestCase >> newTestConnection [

	^RsrMockConnection new
]

{ #category : #accessing }
RsrTestCase >> newTestConnectionUsing: aForwarderClass [

	^(RsrMockConnection forwarderClass: aForwarderClass)
]
