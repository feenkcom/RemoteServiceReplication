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
		'receiver',
		'selector',
		'arguments',
		'snapshots'
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
	servs := snapshots collect: [:each | each reifyIn: aConnection].
	rec := receiver resolve: aConnection registry.
	sel := selector resolve: aConnection registry.
	args := arguments collect: [:each | each resolve: aConnection registry].
	result := rec
		perform: sel
		withArguments: args.
	analysis := RsrSnapshotAnalysis
		roots: (Array with: rec with: result)
		connection: aConnection.
	analysis perform.
	resultReference := RsrReference from: result.
	response := RsrDeliverResponse
		transaction: transaction
		response: resultReference
		snapshots: analysis snapshots.
	aConnection _sendCommand: response]
		on: Error
		do:
			[:ex |
			self
				logException: ex
				to: aConnection log.
			aConnection _sendCommand: (RsrDeliverErrorResponse transaction: transaction remoteError: (RsrRemoteError from: ex))]
]

{ #category : 'reporting' }
RsrSendMessage >> logException: anException
to: aLog [

	| message |
	message := String
		streamContents:
			[:stream |
			stream
				print: receiver;
				nextPutAll: '>>';
				print: selector;
				nextPutAll: ' due to: ';
				nextPutAll: anException description].
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

	aLog debug: 'RsrSendMessage(', self receiver value asString, '>>', self selector value asString, ')'
]

{ #category : 'accessing' }
RsrSendMessage >> selector [
	^ selector
]

{ #category : 'accessing' }
RsrSendMessage >> selector: anObject [
	selector := anObject
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
