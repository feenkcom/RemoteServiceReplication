Class {
	#name : #RsrClientNoInstVars,
	#superclass : #RsrServiceAbstractNoInstVars,
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #'testing-methods' }
RsrClientNoInstVars >> sendReturnNewService [

	^remoteSelf returnNewService
]

{ #category : #'testing-methods' }
RsrClientNoInstVars >> sendReturnNewServiceInArray [

	^remoteSelf returnNewServiceInArray
]

{ #category : #'testing-methods' }
RsrClientNoInstVars >> sendReturnTestSymbol [

	^remoteSelf returnTestSymbol
]

{ #category : #'testing-methods' }
RsrClientNoInstVars >> sendSetMarker [

	^remoteSelf setMarker
]

{ #category : #'testing-methods' }
RsrClientNoInstVars >> sendUnaryReturn42Message [

	^remoteSelf unaryReturn42
]
