Class {
	#name : 'RsrRemotePromiseResolver',
	#superclass : 'RsrObject',
	#instVars : [
		'sendMessage',
		'connection',
		'extraRoots',
		'hasResolved'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrRemotePromiseResolver class >> for: aSendMessage
over: aConnection [

	^self new
		sendMessage: aSendMessage;
		connection: aConnection;
		yourself
]

{ #category : 'accessing' }
RsrRemotePromiseResolver >> addRoot: aService [

	extraRoots add: aService
]

{ #category : 'resolving' }
RsrRemotePromiseResolver >> break: aReason [
	"<aReason> can be any object supported by RSR."

	self connection _sendCommand: (RsrDeliverErrorResponse transaction: self sendMessage transaction remoteError: aReason).
	hasResolved := true
	"self resolution: (Array with: #break with: aReason)"
]

{ #category : 'accessing' }
RsrRemotePromiseResolver >> connection [

	^connection
]

{ #category : 'accessing' }
RsrRemotePromiseResolver >> connection: aConnection [

	connection := aConnection
]

{ #category : 'resolving' }
RsrRemotePromiseResolver >> fulfill: result [
	"Fulfill the remote promise with a fulfilled value of <result>"

	| analysis resultReference response |
	analysis := RsrSnapshotAnalysis
		roots: (Array with: result), extraRoots
		connection: self connection.
	analysis perform.
	resultReference := RsrReference from: result.
	response := RsrDeliverResponse
		transaction: self sendMessage transaction
		responseReference: resultReference
		snapshots: analysis snapshots.
	self connection _sendCommand: response.
	hasResolved := true.
	"self resolution: (Array with: #fulfill with: anObject)"
]

{ #category : 'other' }
RsrRemotePromiseResolver >> hasResolved [

	^hasResolved
]

{ #category : 'private' }
RsrRemotePromiseResolver >> initialize [

	super initialize.
	extraRoots := OrderedCollection new
]

{ #category : 'resolving' }
RsrRemotePromiseResolver >> resolution: aResult [
	"Actually dispatch the result"
]

{ #category : 'accessing' }
RsrRemotePromiseResolver >> sendMessage [

	^sendMessage
]

{ #category : 'accessing' }
RsrRemotePromiseResolver >> sendMessage: aSendMessage [

	sendMessage := aSendMessage
]
