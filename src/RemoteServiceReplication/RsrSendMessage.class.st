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

	true ifTrue: [^self executeFor2: aConnection].
	^[self executeFor2: aConnection]
		on: Error
		do:
			[:ex |
			self trace.
			Transcript
				show: ex messageText;
				cr.
			ex pass]
]

{ #category : 'executing' }
RsrSendMessage >> executeFor2: aConnection [

	| resolver services receiver selector arguments messageSend |
	resolver := RsrRemotePromiseResolver
		for: self
		over: aConnection.
	[services := self snapshots collect: [:each | each reifyIn: aConnection].
	receiver := self receiverReference resolve: aConnection.
	selector := self selectorReference resolve: aConnection.
	arguments := self argumentReferences collect: [:each | each resolve: aConnection]]
		on: Error
		do:
			[:ex |
			self
				logException: ex
				to: aConnection log.
			self note: 'This code path needs to be tested'.
			^resolver break: (RsrRemoteException from: ex)].
	resolver addRoot: receiver. "Ensure we always send back the receiver -- this ensures sending a message results in by-directional syncing."
	messageSend := RsrMessageSend
		receiver: receiver
		selector: selector
		arguments: arguments.
	self
		perform: messageSend
		answerUsing: resolver
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

{ #category : 'executing' }
RsrSendMessage >> perform: aMessageSend
answerUsing: aResolver [

	| raisedException reason |
	raisedException := false.
	[aResolver fulfill: aMessageSend perform]
		on: self unhandledExceptionClass
		do:
			[:ex | | debugResult |
			debugResult := [aMessageSend receiver
				debug: ex
				raisedDuring: aMessageSend
				answerUsing: aResolver]
				on: self unhandledExceptionClass
				do:
					[:debugEx | 
					raisedException := true.
					self
						logException: ex
						to: aResolver connection log.
					reason := (RsrRemoteException from: debugEx).
					ex return].
			aResolver hasResolved
				ifFalse:
					[ex isResumable
						ifTrue: [ex resume: debugResult]
						ifFalse:
							[aResolver break: (RsrRemoteException from: ex).
							ex return]]].
	raisedException
		ifTrue: [aResolver break: reason]
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

{ #category : 'accessing' }
RsrSendMessage >> unhandledExceptionClass [
	"Temporarily, use Error until we have appropriate GemStone hooks."

	^Error
]
