Class {
	#name : 'RsrTokenExchange',
	#superclass : 'RsrHandshakeStep',
	#instVars : [
		'token'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'accessing' }
RsrTokenExchange class >> over: aStream
token: aToken [

	^self new
		stream: aStream;
		token: aToken;
		yourself
]

{ #category : 'accessing' }
RsrTokenExchange class >> token: aToken [

	^self new
		token: aToken;
		yourself
]

{ #category : 'accessing' }
RsrTokenExchange >> codec [

	^RsrTokenExchangeCodec new
]

{ #category : 'accessing' }
RsrTokenExchange >> token [

	^token
]

{ #category : 'accessing' }
RsrTokenExchange >> token: aToken [

	token := aToken
]
