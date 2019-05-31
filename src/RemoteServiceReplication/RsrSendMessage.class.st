Class {
	#name : #RsrSendMessage,
	#superclass : #RsrCommand,
	#instVars : [
		'transaction',
		'receiver',
		'selector',
		'arguments',
		'retainList'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrSendMessage class >> transaction: aTransactionId
receiver: aService
selector: aSelector
arguments: anArray [

	^self new
		transaction: aTransactionId;
		receiver: aService;
		selector: aSelector;
		arguments: anArray;
		yourself
]

{ #category : #accessing }
RsrSendMessage >> arguments [
	^ arguments
]

{ #category : #accessing }
RsrSendMessage >> arguments: anObject [
	arguments := anObject
]

{ #category : #encoding }
RsrSendMessage >> encodeUsing: anEncoder [

	encoding := anEncoder encodeSendMessage: self
]

{ #category : #executing }
RsrSendMessage >> executeFor: aConnection [

	| result response |
	result := receiver
		perform: selector
		withArguments: arguments.
	response := RsrDeliverResponse
		transaction: transaction
		response: result.
	response sendOver: aConnection.
	aConnection objectCache reset
]

{ #category : #accessing }
RsrSendMessage >> receiver [
	^ receiver
]

{ #category : #accessing }
RsrSendMessage >> receiver: anObject [
	receiver := anObject
]

{ #category : #accessing }
RsrSendMessage >> roots [

	^(Array with: receiver with: selector) ,  arguments
]

{ #category : #accessing }
RsrSendMessage >> selector [
	^ selector
]

{ #category : #accessing }
RsrSendMessage >> selector: anObject [
	selector := anObject
]

{ #category : #accessing }
RsrSendMessage >> sendOver: aConnection [

	| analysis promise |
	analysis := RsrRetainAnalysis
		roots: self roots
		connection: aConnection.
	analysis perform.
	retainList := analysis retainCommands.
	self encodeUsing: aConnection encoder.
	promise := RsrPromise new.
	aConnection promises
		at: transaction
		put: promise.
	aConnection commandWriter enqueue: self.
	^promise
]

{ #category : #accessing }
RsrSendMessage >> transaction [
	^ transaction
]

{ #category : #accessing }
RsrSendMessage >> transaction: anObject [
	transaction := anObject
]

{ #category : #writing }
RsrSendMessage >> writeUsing: aCommandWriter [

	retainList do: [:each | each writeUsing: aCommandWriter].
	aCommandWriter write: encoding
]
