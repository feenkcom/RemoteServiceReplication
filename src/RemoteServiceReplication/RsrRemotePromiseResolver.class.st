Class {
	#name : 'RsrRemotePromiseResolver',
	#superclass : 'Object',
	#instVars : [
		'sendMessage',
		'service',
		'connection'
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

{ #category : 'resolving' }
RsrRemotePromiseResolver >> break: aReason [
	"<aReason> can be any object supported by RSR."

	self resolution: (Array with: #break with: aReason)
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
RsrRemotePromiseResolver >> fulfill: anObject [
	"Fulfill the remote promise with a fulfilled value of <anObject>"

	self resolution: (Array with: #fulfill with: anObject)
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
