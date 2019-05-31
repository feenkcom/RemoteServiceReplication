Class {
	#name : #RsrServiceTest,
	#superclass : #RsrObjectTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #accessing }
RsrServiceTest >> newInstance [

	^RsrClientTestService new
]
