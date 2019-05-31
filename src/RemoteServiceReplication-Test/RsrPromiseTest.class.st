Class {
	#name : #RsrPromiseTest,
	#superclass : #TestCase,
	#category : 'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
RsrPromiseTest >> testError [

	| promise expected |
	promise := RsrPromise new.
	expected := Object new.
	[(Delay forSeconds: 1) wait.
	promise error: Error new] fork.
	self
		should: [promise value]
		raise: Error
]

{ #category : #running }
RsrPromiseTest >> testFulfillment [

	| promise expected |
	promise := RsrPromise new.
	expected := Object new.
	[(Delay forSeconds: 1) wait.
	promise fulfill: expected] fork.
	self
		assert: promise value
		identicalTo: expected
]
