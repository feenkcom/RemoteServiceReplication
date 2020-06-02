Class {
	#name : 'RsrChattyService',
	#superclass : 'RsrService',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrChattyService class >> clientClassName [

	^#RsrChattyClient
]

{ #category : 'accessing' }
RsrChattyService class >> serverClassName [

	^#RsrChattyServer
]

{ #category : 'accessing' }
RsrChattyService class >> templateClassName [

	^#RsrChattyService
]
