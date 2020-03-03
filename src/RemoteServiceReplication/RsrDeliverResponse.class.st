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

{ #category : #lifecycle }
RsrDeliverResponse >> encodeUsing: anRsrEncoder [

	encoding := anRsrEncoder encodeDeliverResponse: self
]

{ #category : #accessing }
RsrDeliverResponse >> error [

	^(RsrClassResolver classNamed: errorName ifAbsent: [RsrError]) new
		messageText: response;
		yourself
]

{ #category : #accessing }
RsrDeliverResponse >> errorName [

	^errorName
]

{ #category : #accessing }
RsrDeliverResponse >> errorName: aSymbol [

	errorName := aSymbol
]

{ #category : #lifecycle }
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

{ #category : #testing }
RsrDeliverResponse >> isError [

	^errorName notNil
]

{ #category : #reporting }
RsrDeliverResponse >> reportOn: aLog [

	aLog debug: 'RsrDeliverResponse/', self response class name
]

{ #category : #accessing }
RsrDeliverResponse >> response [

	^response
]

{ #category : #accessing }
RsrDeliverResponse >> response: anObject [

	response := anObject
]

{ #category : #lifecycle }
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

{ #category : #lifecycle }
RsrDeliverResponse >> writeUsing: aCommandWriter [

	retainList do: [:each | each writeUsing: aCommandWriter].
	aCommandWriter write: encoding
]
