Class {
	#name : #RsrServerNoInstVars,
	#superclass : #RsrServiceAbstractNoInstVars,
	#instVars : [
		'marker'
	],
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrServerNoInstVars >> marker [

	^marker ifNil: [false]
]

{ #category : #'testing-methods' }
RsrServerNoInstVars >> returnNewService [

	^RsrServerTestService new
]

{ #category : #'testing-methods' }
RsrServerNoInstVars >> returnNewServiceInArray [

	^Array with: RsrServerTestService new
]

{ #category : #'testing-methods' }
RsrServerNoInstVars >> returnTestSymbol [

	^#testSymbol
]

{ #category : #accessing }
RsrServerNoInstVars >> setMarker [

	marker := true
]

{ #category : #'testing-methods' }
RsrServerNoInstVars >> unaryReturn42 [

	^42
]
