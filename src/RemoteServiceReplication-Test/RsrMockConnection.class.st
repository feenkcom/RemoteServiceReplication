Class {
	#name : 'RsrMockConnection',
	#superclass : 'RsrObject',
	#instVars : [
		'forwarderClass',
		'lastMessage',
		'registry',
		'idSpigot'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'instance creation' }
RsrMockConnection class >> forwarderClass: aClass [ 

	^self new
		forwarderClass: aClass;
		yourself
]

{ #category : 'accessing' }
RsrMockConnection >> _forwarderClass [

	^forwarderClass ifNil: [RsrForwarder]
]

{ #category : 'dispatching' }
RsrMockConnection >> _sendMessage: aMessage
to: anRsrObject [

	lastMessage := RsrSendMessage
		transaction: 1
		receiver: anRsrObject
		selector: aMessage selector
		arguments: aMessage arguments.
	^RsrPromise new
		fulfill: nil;
		yourself
]

{ #category : 'accessing' }
RsrMockConnection >> encoder [

	^RsrMockEncoder new
]

{ #category : 'registering' }
RsrMockConnection >> ensureRegistered: aService [

	aService isMirrored
		ifTrue: [^self].
	aService
		_id: self oidSpigot next
		connection: self
]

{ #category : 'accessing' }
RsrMockConnection >> forwarderClass: aClass [

	forwarderClass := aClass
]

{ #category : 'accessing' }
RsrMockConnection >> lastMessage [

	^[lastMessage]
		ensure: [lastMessage := nil]
]

{ #category : 'accessing' }
RsrMockConnection >> oidSpigot [

	^idSpigot ifNil: [idSpigot := RsrThreadSafeNumericSpigot naturals]
]

{ #category : 'accessing' }
RsrMockConnection >> registry [

	^registry ifNil: [registry := RsrThreadSafeDictionary new]
]

{ #category : 'commands' }
RsrMockConnection >> releaseOid: anInteger [
]
