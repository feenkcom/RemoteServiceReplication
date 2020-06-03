Class {
	#name : 'RsrSendMessage',
	#superclass : 'RsrCommand',
	#instVars : [
		'transaction',
		'receiver',
		'selector',
		'arguments',
		'retainList'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
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

{ #category : 'accessing' }
RsrSendMessage >> arguments [
	^ arguments
]

{ #category : 'accessing' }
RsrSendMessage >> arguments: anObject [
	arguments := anObject
]

{ #category : 'lifecycle' }
RsrSendMessage >> encodeUsing: anEncoder [

	encoding := anEncoder encodeSendMessage: self
]

{ #category : 'lifecycle' }
RsrSendMessage >> executeFor: aConnection [

	| result response |
	[result := receiver
		perform: selector
		withArguments: arguments.
	aConnection objectCache reset.
	response := RsrDeliverResponse
		transaction: transaction
		response: result
		roots: (Array with: receiver with: result).
	response sendOver: aConnection]
		on: Error
		do:
			[:ex |
			self
				logException: ex
				to: aConnection log.
			(RsrDeliverResponse transaction: transaction error: (RsrRemoteError from: ex) roots: #()) sendOver: aConnection]
]

{ #category : 'reporting' }
RsrSendMessage >> logException: anException
to: aLog [

	| message |
	message := String
		streamContents:
			[:stream |
			stream
				nextPutAll: receiver class name;
				nextPutAll: '>>';
				nextPutAll: selector;
				nextPutAll: ' due to: ';
				nextPutAll: anException class name;
				nextPut: $(; nextPutAll: anException messageText asString; nextPut: $)].
	aLog error: message
]

{ #category : 'accessing' }
RsrSendMessage >> receiver [
	^ receiver
]

{ #category : 'accessing' }
RsrSendMessage >> receiver: anObject [
	receiver := anObject
]

{ #category : 'reporting' }
RsrSendMessage >> reportOn: aLog [

	aLog debug: 'RsrSendMessage(', self receiver class name, '>>', self selector, ')'
]

{ #category : 'accessing' }
RsrSendMessage >> roots [

	^(Array with: receiver with: selector) ,  arguments
]

{ #category : 'accessing' }
RsrSendMessage >> selector [
	^ selector
]

{ #category : 'accessing' }
RsrSendMessage >> selector: anObject [
	selector := anObject
]

{ #category : 'lifecycle' }
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

{ #category : 'accessing' }
RsrSendMessage >> transaction [
	^ transaction
]

{ #category : 'accessing' }
RsrSendMessage >> transaction: anObject [
	transaction := anObject
]

{ #category : 'lifecycle' }
RsrSendMessage >> writeUsing: aCommandWriter [

	retainList do: [:each | each writeUsing: aCommandWriter].
	aCommandWriter write: encoding
]
