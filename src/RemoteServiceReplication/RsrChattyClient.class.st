Class {
	#name : 'RsrChattyClient',
	#superclass : 'RsrAbstractChattyService',
	#category : 'RemoteServiceReplication'
}

{ #category : 'accessor' }
RsrChattyClient >> returnArgument: anObject [

	^remoteSelf returnArgument: anObject
]

{ #category : 'accessor' }
RsrChattyClient >> returnSelf [

	^remoteSelf returnSelf
]
