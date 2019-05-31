Class {
	#name : #RsrMockConnection,
	#superclass : #Object,
	#instVars : [
		'forwarderClass',
		'lastMessage',
		'registry',
		'idSpigot'
	],
	#category : 'RemoteServiceReplication-Test-Mocks'
}

{ #category : #'instance creation' }
RsrMockConnection class >> forwarderClass: aClass [ 

	^self new
		forwarderClass: aClass;
		yourself
]

{ #category : #accessing }
RsrMockConnection >> encoder [

	^RsrMockEncoder new
]

{ #category : #accessing }
RsrMockConnection >> forwarderClass [

	^forwarderClass ifNil: [RsrMockForwarder]
]

{ #category : #accessing }
RsrMockConnection >> forwarderClass: aClass [

	forwarderClass := aClass
]

{ #category : #accessing }
RsrMockConnection >> lastMessage [

	^[lastMessage]
		ensure: [lastMessage := nil]
]

{ #category : #accessing }
RsrMockConnection >> oidSpigot [

	^idSpigot ifNil: [idSpigot := RsrThreadSafeNumericSpigot naturals]
]

{ #category : #accessing }
RsrMockConnection >> reaper [

	^RsrObjectReaper connection: self
]

{ #category : #accessing }
RsrMockConnection >> registry [

	^registry ifNil: [registry := RsrRegistry new]
]

{ #category : #commands }
RsrMockConnection >> releaseOid: anInteger [
]

{ #category : #accessing }
RsrMockConnection >> rsrForwarderClass [

	^self forwarderClass
]

{ #category : #dispatching }
RsrMockConnection >> rsrSendMessage: aMessage
to: anRsrObject [

	lastMessage := RsrSendMessage
		transaction: 1
		receiver: anRsrObject
		selector: aMessage selector
		arguments: aMessage arguments.
	^nil
]
