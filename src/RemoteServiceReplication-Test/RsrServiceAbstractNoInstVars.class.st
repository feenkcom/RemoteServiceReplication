Class {
	#name : 'RsrServiceAbstractNoInstVars',
	#superclass : 'RsrService',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrServiceAbstractNoInstVars class >> clientClassName [

	^#RsrClientNoInstVars
]

{ #category : 'accessing' }
RsrServiceAbstractNoInstVars class >> serverClassName [

	^#RsrServerNoInstVars
]

{ #category : 'accessing' }
RsrServiceAbstractNoInstVars >> returnArgument: anObject [

	^anObject
]

{ #category : 'testing-methods' }
RsrServiceAbstractNoInstVars >> sendReturnArgument: anObject [

	^remoteSelf returnArgument: anObject
]
