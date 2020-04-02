Class {
	#name : #RsrAbstractConcurrentTestService,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrAbstractConcurrentTestService class >> clientClassName [

	^#RsrConcurrentTestClient
]

{ #category : #accessing }
RsrAbstractConcurrentTestService class >> serverClassName [

	^#RsrConcurrentTestServer
]
