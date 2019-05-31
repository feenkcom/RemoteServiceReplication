Class {
	#name : #RsrDeliverResponse,
	#superclass : #RsrCommand,
	#instVars : [
		'transaction',
		'errorName',
		'response',
		'retainList'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrDeliverResponse class >> transaction: aTransactionId
error: anException [

	^self new
		transaction: aTransactionId;
		errorName: anException class name;
		response: anException messageText;
		yourself
]

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
RsrDeliverResponse >> error [

	^(Smalltalk at: errorName ifAbsent: [RsrError]) new
		messageText: response;
		yourself
]

{ #category : #executing }
RsrDeliverResponse >> errorName [

	^errorName
]

{ #category : #executing }
RsrDeliverResponse >> errorName: aSymbol [

	errorName := aSymbol
]

{ #category : #executing }
RsrDeliverResponse >> executeFor: aConnection [

	| promise |
	promise := aConnection promises
		removeKey: transaction
		ifAbsent:
			[^self error: 'Handle unknown transaction'].
	self isError
		ifTrue: [promise error: self error]
		ifFalse: [promise fulfill: response].
	aConnection objectCache reset
	
]

{ #category : #encoding }
RsrDeliverResponse >> isError [

	^errorName notNil
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
