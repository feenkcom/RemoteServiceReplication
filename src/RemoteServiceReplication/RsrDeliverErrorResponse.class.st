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
RsrDeliverErrorResponse >> encodeUsing: anRsrEncoder [

	encoding := anRsrEncoder encodeDeliverErrorResponse: self
]

{ #category : 'lifecycle' }
RsrDeliverErrorResponse >> executeFor: aConnection [

	| promise |
	promise := aConnection promises
		removeKey: transaction
		ifAbsent: [^self error: 'Handle unknown transaction'].
	promise error: self remoteError
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

	self encodeUsing: aConnection encoder.
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
