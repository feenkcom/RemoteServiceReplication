"
I represent a class of Objects that know offer Rsr Services.
"
Class {
	#name : #RsrService,
	#superclass : #RsrAbstractService,
	#instVars : [
		'_id',
		'_connection',
		'remoteSelf'
	],
	#category : #'RemoteServiceReplication-Public'
}

{ #category : #'instance creation' }
RsrService class >> _id: anId
connection: aConnection [

	^super new
		_id: anId connection: aConnection;
		yourself
]

{ #category : #accessing }
RsrService class >> clientClass [

	self subclassResponsibility
]

{ #category : #testing }
RsrService class >> isClientClass [

	^self == self clientClass
]

{ #category : #testing }
RsrService class >> isServerClass [

	^self == self serverClass
]

{ #category : #accessing }
RsrService class >> serverClass [

	self subclassResponsibility
]

{ #category : #'private-accessing' }
RsrService class >> variablesToReflect [

	| currentClass variables |
	variables := OrderedCollection new.
	currentClass := self superclass.
	[currentClass == RsrService]
		whileFalse:
			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].
			currentClass := currentClass superclass].
	^variables
]

{ #category : #'private-lifecycle' }
RsrService >> _addTo: aRegistry [

	aRegistry
		at: _id
		put: self
]

{ #category : #'private-accessing' }
RsrService >> _connection [

	^_connection
]

{ #category : #'private-accessing' }
RsrService >> _id [

	^_id
]

{ #category : #'private-accessing' }
RsrService >> _id: anRsrId
connection: aConnection [

	_id := anRsrId.
	_connection := aConnection.
	remoteSelf := aConnection _forwarderClass on: self
]

{ #category : #'private-enumerating' }
RsrService >> _reflectedVariableIndecesDo: aBlock [

	| allVariables |
	allVariables := self class allInstVarNames.
	self class variablesToReflect
		do:
			[:varName | | index |
			index := allVariables indexOf: varName.
			aBlock value: index]
]

{ #category : #'private-enumerating' }
RsrService >> _reflectedVariablesDo: aBlock [

	self _reflectedVariableIndecesDo: [:index | aBlock value: (self instVarAt: index)]
]

{ #category : #'private-synchronization' }
RsrService >> _synchronize [
	"Return self to synchronize with the remote peer"

	^self
]

{ #category : #'public-accessing' }
RsrService >> isClient [

	^self class isClientClass
]

{ #category : #'public-testing' }
RsrService >> isMirrored [

	^_connection ~~ nil
]

{ #category : #'public-testing' }
RsrService >> isNotMirrored [

	^self isMirrored not
]

{ #category : #'public-accessing' }
RsrService >> isServer [

	^self class isServerClass
]

{ #category : #'public-accessing' }
RsrService >> remoteServiceClass [

	^self isClient
		ifTrue: [self class serverClass]
		ifFalse: [self class clientClass]
]

{ #category : #'public-accessing' }
RsrService >> remoteServiceName [

	^self remoteServiceClass name
]

{ #category : #'public-accessing' }
RsrService >> serviceName [

	^self class name
]

{ #category : #'public-synchronization' }
RsrService >> synchronize [

	remoteSelf == nil
		ifFalse: [remoteSelf _synchronize]
]

{ #category : #'public-accessing' }
RsrService >> variablesToReflect [

	^self class variablesToReflect
]
