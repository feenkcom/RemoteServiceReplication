Class {
	#name : 'RsrTestingConcurrencyTestCase',
	#superclass : 'RsrTestCase',
	#category : 'RemoteServiceReplication-Compatibility-Test'
}

{ #category : 'running' }
RsrTestingConcurrencyTestCase >> exceptionCase [

	| sema |
	sema := Semaphore new.
	RsrConcurrency fork: [[Error signal] ensure: [sema signal]].
	sema wait
]

{ #category : 'running' }
RsrTestingConcurrencyTestCase >> noExceptionCase [

	| sema |
	sema := Semaphore new.
	RsrConcurrency fork: [sema signal].
	sema wait
]

{ #category : 'running' }
RsrTestingConcurrencyTestCase >> testException [

	| testCase |
	testCase := self class selector: #exceptionCase.
	self
		should: [testCase runCase]
		raise: Exception
]

{ #category : 'running' }
RsrTestingConcurrencyTestCase >> testNoException [

	| testCase |
	testCase := self class selector: #noExceptionCase.
	self
		shouldnt: [testCase runCase]
		raise: Exception
]
