Class {
	#name : #RsrObjectTestCase,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #testing }
RsrObjectTestCase class >> isAbstract [

	^self == RsrObjectTestCase
]

{ #category : #accessing }
RsrObjectTestCase >> newInstance [

	self subclassResponsibility
]

{ #category : #running }
RsrObjectTestCase >> testInitialization [

	| instance |
	instance := self newInstance.
	self
		assert: instance isMirrored
		equals: false.
	self
		assert: instance _id
		equals: nil.
	self
		assert: instance _connection
		equals: nil
]

{ #category : #running }
RsrObjectTestCase >> testIsMirrored [

	| instance |
	instance := self newInstance.
	self deny: instance isMirrored.
	instance
		_id: 1;
		_connection: RsrMockConnection new.
	self assert: instance isMirrored
]
