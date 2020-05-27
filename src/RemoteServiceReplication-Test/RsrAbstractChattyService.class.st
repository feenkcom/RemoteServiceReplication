Class {
	#name : 'RsrAbstractChattyService',
	#superclass : 'RsrService',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrAbstractChattyService class >> abstractClassName [

	^#RsrAbstractChattyService
]

{ #category : 'accessing' }
RsrAbstractChattyService class >> clientClassName [

	^#RsrChattyClient
]

{ #category : 'accessing' }
RsrAbstractChattyService class >> serverClassName [

	^#RsrChattyServer
]
