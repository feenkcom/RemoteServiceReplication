"
No class-specific documentation for RsrDeliverResponse, hierarchy is:
Object
  RsrObject
    RsrCommand( encoding)
      RsrDeliverResponse( transaction response roots retainList)

"
Class {
	#name : 'RsrDeliverResponse',
	#superclass : 'RsrCommand',
	#instVars : [
		'transaction',
		'response',
		'roots',
		'snapshots',
		'retainList'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrDeliverResponse class >> transaction: aTransactionId
error: anException
roots: anArray [

	^self new
		transaction: aTransactionId;
		errorName: anException class name;
		response: anException messageText;
		roots: anArray;
		yourself
]

{ #category : 'instance creation' }
RsrDeliverResponse class >> transaction: aTransactionId
response: anObject
roots: anArray [

	^self new
		transaction: aTransactionId;
		response: anObject;
		roots: anArray;
		yourself
]

{ #category : 'instance creation' }
RsrDeliverResponse class >> transaction: aTransactionId
response: anObject
snapshots: anArray [

	^self new
		transaction: aTransactionId;
		response: anObject;
		snapshots: anArray;
		yourself
]

{ #category : 'lifecycle' }
RsrDeliverResponse >> encodeUsing: anRsrEncoder [

	encoding := anRsrEncoder encodeDeliverResponse: self
]

{ #category : 'lifecycle' }
RsrDeliverResponse >> executeFor: aConnection [

	| pendingMessage result |
	snapshots do: [:each | each reifyIn: aConnection].
	result := response resolve: aConnection registry.
	pendingMessage := aConnection pendingMessages
		removeKey: transaction
		ifAbsent:
			[^self error: 'Handle unknown transaction'].
	pendingMessage promise fulfill: result
]

{ #category : 'reporting' }
RsrDeliverResponse >> reportOn: aLog [

	aLog debug: 'RsrDeliverResponse(', self response class name, ')'
]

{ #category : 'accessing' }
RsrDeliverResponse >> response [

	^response
]

{ #category : 'accessing' }
RsrDeliverResponse >> response: anObject [

	response := anObject
]

{ #category : 'accessing' }
RsrDeliverResponse >> roots [

	^roots
]

{ #category : 'accessing' }
RsrDeliverResponse >> roots: anArray [

	roots := anArray
]

{ #category : 'lifecycle' }
RsrDeliverResponse >> sendOver: aConnection [

	self encodeUsing: aConnection encoder.
	aConnection commandWriter enqueue: self
]

{ #category : 'accessing' }
RsrDeliverResponse >> snapshots [

	^snapshots
]

{ #category : 'accessing' }
RsrDeliverResponse >> snapshots: anArrayOfSnapshots [

	snapshots := anArrayOfSnapshots
]

{ #category : 'accessing' }
RsrDeliverResponse >> transaction [

	^transaction
]

{ #category : 'accessing' }
RsrDeliverResponse >> transaction: aTransactionId [

	transaction := aTransactionId
]
