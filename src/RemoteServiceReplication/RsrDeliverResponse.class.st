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
		'responseReference',
		'snapshots'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrDeliverResponse class >> transaction: aTransactionId
responseReference: aReference
snapshots: anArrayOfSnapshots [

	^self new
		transaction: aTransactionId;
		responseReference: aReference;
		snapshots: anArrayOfSnapshots;
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

	| pendingMessage result strongServices |
	pendingMessage := aConnection pendingMessages
		removeKey: self transaction
		ifAbsent: [^self reportUnknownTransactionIn: aConnection].
	"Must keep a strong reference to each service until we're sure a parent service is reified"
	[strongServices := self snapshots collect: [:each | each reifyIn: aConnection].
	result := self responseReference resolve: aConnection.
	result first == #fulfill
		ifTrue: [pendingMessage promise fulfill: result last]
		ifFalse: [pendingMessage promise break: result last]]
		on: Error
		do: [:ex | pendingMessage promise break: (RsrDecodingRaisedException exception: Exception)]
]

{ #category : 'reporting' }
RsrDeliverResponse >> reportOn: aLog [

	aLog debug: 'RsrDeliverResponse(', self response class name, ')'
]

{ #category : 'reporting' }
RsrDeliverResponse >> reportUnknownTransactionIn: aConnection [

	aConnection log error: 'Unknown transaction (', self transaction asString, ') while processing Response'
]

{ #category : 'accessing' }
RsrDeliverResponse >> response [

	^self responseReference
]

{ #category : 'accessing' }
RsrDeliverResponse >> response: anObject [

	^self responseReference: anObject
]

{ #category : 'accessing' }
RsrDeliverResponse >> responseReference [

	^responseReference
]

{ #category : 'accessing' }
RsrDeliverResponse >> responseReference: aReference [

	responseReference := aReference
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
