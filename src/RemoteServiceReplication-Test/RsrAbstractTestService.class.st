Class {
	#name : #RsrAbstractTestService,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrAbstractTestService class >> clientClass [

	^RsrClientTestService
]

{ #category : #accessing }
RsrAbstractTestService class >> serverClass [

	^RsrServerTestService
]

{ #category : #accessing }
RsrAbstractTestService >> remoteSelf [

	^remoteSelf
]
