Class {
	#name : 'RsrDeliverErrorResponse',
	#superclass : 'RsrCommand',
	#instVars : [
		'transaction',
		'originalClass',
		'remoteError'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrDeliverErrorResponse class >> transaction: aTransactionId
remoteError: anException [

	^self new
		transaction: aTransactionId;
		remoteError: anException;
		yourself
]

{ #category : 'lifecycle' }
RsrDeliverErrorResponse >> executeFor: aConnection [

	| pendingMessage |
	pendingMessage := aConnection pendingMessages
		removeKey: transaction
		ifAbsent: [^self error: 'Handle unknown transaction'].
	pendingMessage promise error: self remoteError
]

{ #category : 'accessing' }
RsrDeliverErrorResponse >> remoteError [

	^remoteError
]

{ #category : 'accessing' }
RsrDeliverErrorResponse >> remoteError: aRemoteError [

	remoteError := aRemoteError
]

{ #category : 'reporting' }
RsrDeliverErrorResponse >> reportOn: aLog [

	aLog debug: 'RsrDeliverErrorResponse(', self remoteError class name, ')'
]

{ #category : 'lifecycle' }
RsrDeliverErrorResponse >> sendOver: aConnection [

	encoding := aConnection encoder encodeDeliverErrorResponse: self.
	aConnection commandWriter enqueue: self
]

{ #category : 'accessing' }
RsrDeliverErrorResponse >> transaction [

	^transaction
]

{ #category : 'accessing' }
RsrDeliverErrorResponse >> transaction: anInteger [

	transaction := anInteger
]
