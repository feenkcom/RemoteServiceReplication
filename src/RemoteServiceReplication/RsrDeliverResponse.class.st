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
		'snapshots'
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

{ #category : 'encoding' }
RsrDeliverResponse >> encode: aStream
using: anEncoder [

	anEncoder
		encodeDeliverResponse: self
		onto: aStream
]

{ #category : 'executing' }
RsrDeliverResponse >> executeFor: aConnection [

	| pendingMessage result |
	pendingMessage := aConnection pendingMessages
		removeKey: transaction
		ifAbsent:
			[^self error: 'Handle unknown transaction'].
	[snapshots do: [:each | each reifyIn: aConnection].
	result := response resolve: aConnection registry.
	pendingMessage promise fulfill: result]
		on: Error
		do: [:ex | pendingMessage promise fulfill: ex copy]
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
