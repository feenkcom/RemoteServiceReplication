Class {
	#name : #RsrDeliverResponse,
	#superclass : #RsrCommand,
	#instVars : [
		'transaction',
		'response',
		'retainList'
	],
	#category : 'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrDeliverResponse class >> transaction: aTransactionId
response: anObject [

	^self new
		transaction: aTransactionId;
		response: anObject;
		yourself
]

{ #category : #encoding }
RsrDeliverResponse >> encodeUsing: anRsrEncoder [

	encoding := anRsrEncoder encodeDeliverResponse: self
]

{ #category : #executing }
RsrDeliverResponse >> executeFor: aConnection [

	| promise |
	promise := aConnection promises
		removeKey: transaction
		ifAbsent:
			[^self error: 'Handle unknown transaction'].
	promise fulfill: response.
	aConnection objectCache reset
	
]

{ #category : #accessing }
RsrDeliverResponse >> response [

	^response
]

{ #category : #accessing }
RsrDeliverResponse >> response: anObject [

	response := anObject
]

{ #category : #encoding }
RsrDeliverResponse >> sendOver: aConnection [

	| analysis |
	analysis := RsrRetainAnalysis
		roots: (Array with: response)
		connection: aConnection.
	analysis perform.
	retainList := analysis retainCommands.
	self encodeUsing: aConnection encoder.
	aConnection commandWriter enqueue: self
]

{ #category : #accessing }
RsrDeliverResponse >> transaction [

	^transaction
]

{ #category : #accessing }
RsrDeliverResponse >> transaction: aTransactionId [

	transaction := aTransactionId
]

{ #category : #encoding }
RsrDeliverResponse >> writeUsing: aCommandWriter [

	retainList do: [:each | each writeUsing: aCommandWriter].
	aCommandWriter write: encoding
]
