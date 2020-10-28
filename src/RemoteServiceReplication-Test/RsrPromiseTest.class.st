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
RsrPromiseTest >> testAsyncBreak [

	| promise semaphore expected whenRan first second third |
	promise := RsrPromise new.
	semaphore := Semaphore new.
	expected := Object new.
	whenRan := false.
	promise
		when: [:object | whenRan := true. semaphore signal]
		catch: [:reason | first := reason. semaphore signal].
	promise
		when: [:object | whenRan := true. semaphore signal]
		catch: [:reason | second := reason. semaphore signal].
	self
		deny: promise isResolved;
		deny: promise isBroken;
		deny: promise isFulfilled.
	promise break: expected.
	self
		assert: promise isResolved;
		assert: promise isBroken;
		deny: promise isFulfilled.
	semaphore wait; wait.
	self shortWait. "Ensure any when blocks run if they are going to schedule."
	self deny: whenRan.
	self
		assert: first
		identicalTo: expected.
	self
		assert: second
		identicalTo: expected.
	promise
		when: [:object | whenRan := true. semaphore signal]
		catch: [:reason | third := reason. semaphore signal].
	semaphore wait.
	self shortWait.
	self deny: whenRan.
	self
		assert: third
		identicalTo: expected
]

{ #category : 'running' }
RsrPromiseTest >> testAsyncFulfill [

	| promise semaphore expected catchRan first second third |
	promise := RsrPromise new.
	semaphore := Semaphore new.
	expected := Object new.
	catchRan := false.
	promise
		when: [:object | first := object. semaphore signal]
		catch: [:reason | catchRan := true. semaphore signal].
	promise
		when: [:object | second := object. semaphore signal]
		catch: [:reason | catchRan := true. semaphore signal].
	self
		deny: promise isResolved;
		deny: promise isBroken;
		deny: promise isFulfilled.
	promise fulfill: expected.
	self
		assert: promise isResolved;
		deny: promise isBroken;
		assert: promise isFulfilled.
	semaphore wait; wait.
	self shortWait. "Ensure any catch blocks run if they are going to schedule."
	self deny: catchRan.
	self
		assert: first
		identicalTo: expected.
	self
		assert: second
		identicalTo: expected.
	promise
		when: [:object | third := object. semaphore signal]
		catch: [:reason | catchRan := true. semaphore signal].
	semaphore wait.
	self shortWait.
	self deny: catchRan.
	self
		assert: third
		identicalTo: expected
]

{ #category : 'running' }
RsrPromiseTest >> testSyncBreak [

	| promise expected exceptionRaised first second third |
	promise := RsrPromise new.
	expected := Object new.
	exceptionRaised := false.
	self fork: [[promise wait] on: RsrBrokenPromise do: [:ex | exceptionRaised := true. first := ex reason. ex return]].
	self fork: [[promise wait] on: RsrBrokenPromise do: [:ex | exceptionRaised := true. second := ex reason. ex return]].
	promise break: expected.
	self shortWait. "Allow results to process."
	self assert: exceptionRaised.
	self
		assert: first
		identicalTo: expected.
	self
		assert: second
		identicalTo: expected.
	self
		should: [promise wait]
		raise: RsrBrokenPromise.
	third := [promise wait]
		on: RsrBrokenPromise
		do: [:ex | ex return: ex reason].
	self
		assert: third
		identicalTo: expected
]

{ #category : 'running' }
RsrPromiseTest >> testSyncFulfill [

	| promise expected exceptionRaised first second |
	promise := RsrPromise new.
	expected := Object new.
	exceptionRaised := false.
	self fork: [[first := promise wait] on: RsrBrokenPromise do: [:ex | exceptionRaised := true. ex return]].
	self fork: [[second := promise wait] on: RsrBrokenPromise do: [:ex | exceptionRaised := true. ex return]].
	promise fulfill: expected.
	self shortWait. "Allow results to process."
	self deny: exceptionRaised.
	self
		assert: first
		identicalTo: expected.
	self
		assert: second
		identicalTo: expected.
	self
		assert: promise wait
		identicalTo: expected
]
