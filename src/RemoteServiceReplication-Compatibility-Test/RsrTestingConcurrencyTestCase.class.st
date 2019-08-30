Class {
	#name : #RsrTestingConcurrencyTestCase,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Compatibility-Test'
}

{ #category : #running }
RsrTestingConcurrencyTestCase >> exceptionCase [

	RsrConcurrency
		fork: [Error signal]
		at: Processor highIOPriority
]

{ #category : #running }
RsrTestingConcurrencyTestCase >> noExceptionCase [

	| process sema |
	sema := Semaphore new.
	process := RsrConcurrency fork: [sema signal].
	sema wait
]

{ #category : #running }
RsrTestingConcurrencyTestCase >> testException [

	| testCase |
	testCase := self class selector: #exceptionCase.
	self
		should: [testCase runCase]
		raise: Exception
]

{ #category : #running }
RsrTestingConcurrencyTestCase >> testNoException [

	| testCase |
	testCase := self class selector: #noExceptionCase.
	self
		shouldnt: [testCase runCase]
		raise: Exception
]
