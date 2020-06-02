Class {
	#name : 'RsrServerNoInstVars',
	#superclass : 'RsrServiceNoInstVars',
	#instVars : [
		'marker'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrServerNoInstVars >> marker [

	^marker ifNil: [false]
]

{ #category : 'testing-methods' }
RsrServerNoInstVars >> returnNewService [

	^RsrServerTestService new
]

{ #category : 'testing-methods' }
RsrServerNoInstVars >> returnNewServiceInArray [

	^Array with: RsrServerTestService new
]

{ #category : 'testing-methods' }
RsrServerNoInstVars >> returnObjectNew [

	^Object new
]

{ #category : 'testing-methods' }
RsrServerNoInstVars >> returnSelf [

	^self
]

{ #category : 'testing-methods' }
RsrServerNoInstVars >> returnTestSymbol [

	^#testSymbol
]

{ #category : 'accessing' }
RsrServerNoInstVars >> setMarker [

	marker := true
]

{ #category : 'testing-methods' }
RsrServerNoInstVars >> signalError [

	^Error signal
]

{ #category : 'testing-methods' }
RsrServerNoInstVars >> unaryReturn42 [

	^42
]
