Class {
	#name : #RsrServiceAbstractNoInstVars,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrServiceAbstractNoInstVars class >> clientClass [

	^RsrClientNoInstVars
]

{ #category : #accessing }
RsrServiceAbstractNoInstVars class >> serverClass [

	^RsrServerNoInstVars
]

{ #category : #accessing }
RsrServiceAbstractNoInstVars >> returnArgument: anObject [

	^anObject
]

{ #category : #'testing-methods' }
RsrServiceAbstractNoInstVars >> sendReturnArgument: anObject [

	^remoteSelf returnArgument: anObject
]
