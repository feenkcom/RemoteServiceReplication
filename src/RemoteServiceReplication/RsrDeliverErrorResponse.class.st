"
No class-specific documentation for RsrDeliverErrorResponse, hierarchy is:
Object
  RsrObject
    RsrCommand( encoding)
      RsrDeliverErrorResponse( transaction originalClass remoteError)

"
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

{ #category : 'encoding' }
RsrDeliverErrorResponse >> encode: aStream
using: anEncoder [

	anEncoder
		encodeDeliverErrorResponse: self
		onto: aStream
]

{ #category : 'executing' }
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

{ #category : 'accessing' }
RsrDeliverErrorResponse >> transaction [

	^transaction
]

{ #category : 'accessing' }
RsrDeliverErrorResponse >> transaction: anInteger [

	transaction := anInteger
]
