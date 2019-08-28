Class {
	#name : #RsrServiceTest,
	#superclass : #RsrObjectTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #accessing }
RsrServiceTest >> newInstance [

	^RsrClientTestService new
]

{ #category : #running }
RsrServiceTest >> testHasRemoteSelf [

	| service |
	service := self newInstance.
	service
		_id: 1;
		_connection: RsrMockConnection new.
	self deny: nil == service remoteSelf
]
