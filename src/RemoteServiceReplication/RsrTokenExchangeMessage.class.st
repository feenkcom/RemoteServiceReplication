Class {
	#name : 'RsrTokenExchangeMessage',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication'
}

{ #category : 'comparing' }
RsrTokenExchangeMessage >> = aTokenExchangeMessage [

	^self class == aTokenExchangeMessage class
]

{ #category : 'hashing' }
RsrTokenExchangeMessage >> hash [

	^self class hash
]
