Class {
	#name : 'RsrAbstractChattyService',
	#superclass : 'RsrService',
	#category : 'RemoteServiceReplication'
}

{ #category : 'accessing' }
RsrAbstractChattyService class >> clientClassName [

	^#RsrChattyClient
]

{ #category : 'accessing' }
RsrAbstractChattyService class >> serverClassName [

	^#RsrChattyServer
]
