Class {
	#name : #RsrPromiseTest,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
RsrPromiseTest >> testError [

	| promise semaphore |
	promise := RsrPromise new.
	self fork:
		[(Delay forSeconds: 1) wait.
		promise error: Error new].
	self
		should: [promise value]
		raise: Error.
	promise := RsrPromise new.
	semaphore := Semaphore new.
	self fork:
		[promise error: Error new.
		semaphore signal].
	self
		should: [promise value]
		raise: Error
]

{ #category : #running }
RsrPromiseTest >> testFulfillment [

	| promise expected semaphore |
	promise := RsrPromise new.
	expected := Object new.
	self fork:
		[(Delay forSeconds: 1) wait.
		promise fulfill: expected].
	self
		assert: promise value
		identicalTo: expected.
	promise := RsrPromise new.
	semaphore := Semaphore new.
	[promise fulfill: expected.
	semaphore signal] fork.
	semaphore wait.
	self
		assert: promise value
		identicalTo: expected
]
