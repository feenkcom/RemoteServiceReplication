Class {
	#name : 'RsrDeliverResponse',
	#superclass : 'RsrCommand',
	#instVars : [
		'transaction',
		'response',
		'roots',
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

{ #category : 'lifecycle' }
RsrDeliverResponse >> encodeUsing: anRsrEncoder [

	encoding := anRsrEncoder encodeDeliverResponse: self
]

{ #category : 'lifecycle' }
RsrDeliverResponse >> executeFor: aConnection [

	| pendingMessage |
	pendingMessage := aConnection pendingMessages
		removeKey: transaction
		ifAbsent:
			[^self error: 'Handle unknown transaction'].
	pendingMessage promise fulfill: response.
	aConnection objectCache reset
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

	| analysis |
	analysis := RsrRetainAnalysis
		roots: roots
		connection: aConnection.
	analysis perform.
	retainList := analysis retainCommands.
	self encodeUsing: aConnection encoder.
	aConnection commandWriter enqueue: self
]

{ #category : 'accessing' }
RsrDeliverResponse >> transaction [

	^transaction
]

{ #category : 'accessing' }
RsrDeliverResponse >> transaction: aTransactionId [

	transaction := aTransactionId
]

{ #category : 'lifecycle' }
RsrDeliverResponse >> writeUsing: aCommandWriter [

	retainList do: [:each | each writeUsing: aCommandWriter].
	aCommandWriter write: encoding
]
