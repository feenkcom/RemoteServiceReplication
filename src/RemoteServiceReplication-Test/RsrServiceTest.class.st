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
		rsrId: 1;
		rsrConnection: RsrMockConnection new.
	self
		deny: service remoteSelf
		equals: nil
]
