"
No class-specific documentation for RsrSendMessage, hierarchy is:
Object
  RsrObject
    RsrCommand( encoding)
      RsrSendMessage( transaction receiver selector arguments retainList)

"
Class {
	#name : 'RsrSendMessage',
	#superclass : 'RsrCommand',
	#instVars : [
		'transaction',
		'receiverReference',
		'selectorReference',
		'argumentReferences',
		'snapshots'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrSendMessage class >> transaction: aTransactionId
receiverReference: aServiceReference
selectorReference: aSelectorReference
argumentReferences: anArrayOfReferences [

	^self new
		transaction: aTransactionId;
		receiverReference: aServiceReference;
		selectorReference: aSelectorReference;
		argumentReferences: anArrayOfReferences;
		yourself
]

{ #category : 'accessing' }
RsrSendMessage >> argumentReferences [

	^argumentReferences
]

{ #category : 'accessing' }
RsrSendMessage >> argumentReferences: anArrayOfReferences [

	argumentReferences := anArrayOfReferences
]

{ #category : 'encoding' }
RsrSendMessage >> encode: aStream
using: anEncoder [

	anEncoder
		encodeSendMessage: self
		onto: aStream
]

{ #category : 'executing' }
RsrSendMessage >> executeFor: aConnection [

	| result analysis resultReference response |
	[| servs rec sel args |
	servs := self snapshots collect: [:each | each reifyIn: aConnection].
	rec := self receiverReference resolve: aConnection.
	sel := self selectorReference resolve: aConnection.
	args := self argumentReferences collect: [:each | each resolve: aConnection].
	result := rec
		perform: sel
		withArguments: args.
	analysis := RsrSnapshotAnalysis
		roots: (Array with: rec with: result)
		connection: aConnection.
	analysis perform.
	resultReference := RsrReference from: result.
	response := RsrDeliverResponse
		transaction: self transaction
		responseReference: resultReference
		snapshots: analysis snapshots.
	aConnection _sendCommand: response]
		on: Error
		do:
			[:ex |
			self
				logException: ex
				to: aConnection log.
			aConnection _sendCommand: (RsrDeliverErrorResponse transaction: self transaction remoteError: (RsrRemoteError from: ex))]
]

{ #category : 'reporting' }
RsrSendMessage >> logException: anException
to: aLog [

	| message |
	message := String
		streamContents:
			[:stream |
			stream
				print: self receiverReference;
				nextPutAll: '>>';
				print: self selectorReference;
				nextPutAll: ' due to: ';
				nextPutAll: anException description].
	aLog error: message
]

{ #category : 'accessing' }
RsrSendMessage >> receiverReference [

	^receiverReference
]

{ #category : 'accessing' }
RsrSendMessage >> receiverReference: aServiceReference [

	receiverReference := aServiceReference
]

{ #category : 'reporting' }
RsrSendMessage >> reportOn: aLog [

	aLog debug: 'RsrSendMessage(', self receiverReference asString, '>>', self selectorReference asString, ')'
]

{ #category : 'accessing' }
RsrSendMessage >> selectorReference [

	^selectorReference
]

{ #category : 'accessing' }
RsrSendMessage >> selectorReference: aSymbolReference [

	selectorReference := aSymbolReference
]

{ #category : 'accessing' }
RsrSendMessage >> snapshots [

	^snapshots
]

{ #category : 'accessing' }
RsrSendMessage >> snapshots: anArrayOfSnapshots [

	snapshots := anArrayOfSnapshots
]

{ #category : 'accessing' }
RsrSendMessage >> transaction [
	^ transaction
]

{ #category : 'accessing' }
RsrSendMessage >> transaction: anObject [
	transaction := anObject
]
