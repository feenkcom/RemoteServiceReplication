Class {
	#name : 'RsrAbstractConcurrentTestService',
	#superclass : 'RsrService',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrAbstractConcurrentTestService class >> abstractClassName [

	^#RsrAbstractConcurrentTestService
]

{ #category : 'accessing' }
RsrAbstractConcurrentTestService class >> clientClassName [

	^#RsrConcurrentTestClient
]

{ #category : 'accessing' }
RsrAbstractConcurrentTestService class >> serverClassName [

	^#RsrConcurrentTestServer
]
