Class {
	#name : 'RsrPromiseTest',
	#superclass : 'RsrTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'utilities' }
RsrPromiseTest >> shortWait [

	(Delay forMilliseconds: 100) wait
]

{ #category : 'running' }
RsrPromiseTest >> testWaitBreak [

	| promise reason exception |
	promise := RsrPromise new.
	reason := #testWaitBreak.
	self deny: promise isResolved.
	self
		fork:
			[self shortWait.
			promise break: reason].
	self
		should: [promise wait]
		raise: RsrBrokenPromise.
	"Does it raise multiple times?
	And assertions about exception"
	exception := [promise wait]
		on: RsrBrokenPromise
		do: [:ex | ex return: ex].
	self
		assert: exception class
		equals: RsrBrokenPromise.
	self
		assert: exception reason
		equals: reason.
	self
		assert: promise isResolved;
		assert: promise isBroken;
		deny: promise isFulfilled
]

{ #category : 'running' }
RsrPromiseTest >> testWaitFulfilled [

	| promise expected |
	promise := RsrPromise new.
	expected := #testWaitFulfilled.
	self deny: promise isResolved.
	self
		fork:
			[self shortWait.
			promise fulfill: expected].
	self
		assert: promise wait
		equals: expected.
	"Ensure waiting multiple times gives same result."
	self
		assert: promise wait
		equals: expected.
	self
		assert: promise isResolved;
		assert: promise isFulfilled;
		deny: promise isBroken
]
