"
I represent a class of Objects that know offer Rsr Services.
"
Class {
	#name : 'RsrService',
	#superclass : 'RsrAbstractService',
	#instVars : [
		'_id',
		'_connection',
		'remoteSelf'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrService class >> _id: anId
connection: aConnection [

	^super new
		_id: anId connection: aConnection;
		yourself
]

{ #category : 'private-accessing' }
RsrService class >> _variablesToReflect [

	| currentClass variables |
	variables := OrderedCollection new.
	currentClass := self abstractClass.
	[currentClass == RsrService]
		whileFalse:
			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].
			currentClass := currentClass superclass].
	^variables
]

{ #category : 'accessing' }
RsrService class >> abstractClass [

	^RsrClassResolver classNamed: self abstractClassName
]

{ #category : 'accessing' }
RsrService class >> abstractClassName [

	self subclassResponsibility
]

{ #category : 'accessing' }
RsrService class >> clientClass [

	^RsrClassResolver classNamed: self clientClassName
]

{ #category : 'accessing' }
RsrService class >> clientClassName [

	self subclassResponsibility
]

{ #category : 'testing' }
RsrService class >> isAbstractClass [

	^self name == self abstractClassName
]

{ #category : 'testing' }
RsrService class >> isClientClass [

	^self name == self clientClassName
]

{ #category : 'testing' }
RsrService class >> isServerClass [

	^self name == self serverClassName
]

{ #category : 'accessing' }
RsrService class >> serverClass [

	^RsrClassResolver classNamed: self serverClassName
]

{ #category : 'accessing' }
RsrService class >> serverClassName [

	self subclassResponsibility
]

{ #category : 'private-lifecycle' }
RsrService >> _addTo: aRegistry [

	aRegistry
		serviceAt: _id
		put: self
]

{ #category : 'private-accessing' }
RsrService >> _connection [

	^_connection
]

{ #category : 'private-accessing' }
RsrService >> _id [

	^_id
]

{ #category : 'private-accessing' }
RsrService >> _id: anRsrId
connection: aConnection [

	_id := anRsrId.
	_connection := aConnection.
	remoteSelf := aConnection _forwarderClass on: self
]

{ #category : 'private-enumerating' }
RsrService >> _reflectedVariableIndecesDo: aBlock [

	| allVariables |
	allVariables := self class allInstVarNames.
	self class _variablesToReflect
		do:
			[:varName | | index |
			index := allVariables indexOf: varName.
			aBlock value: index]
]

{ #category : 'private-enumerating' }
RsrService >> _reflectedVariablesDo: aBlock [

	self _reflectedVariableIndecesDo: [:index | aBlock value: (self instVarAt: index)]
]

{ #category : 'private-synchronization' }
RsrService >> _synchronize [
	"Return self to synchronize with the remote peer"

	^self
]

{ #category : 'public-accessing' }
RsrService >> _variablesToReflect [

	^self class _variablesToReflect
]

{ #category : 'public-accessing' }
RsrService >> isClient [

	^self class isClientClass
]

{ #category : 'public-testing' }
RsrService >> isMirrored [

	^_connection ~~ nil
]

{ #category : 'public-testing' }
RsrService >> isNotMirrored [

	^self isMirrored not
]

{ #category : 'public-accessing' }
RsrService >> isServer [

	^self class isServerClass
]

{ #category : 'public-accessing' }
RsrService >> remoteServiceName [

	^self isClient
		ifTrue: [self class serverClassName]
		ifFalse: [self class clientClassName]
]

{ #category : 'public-accessing' }
RsrService >> serviceName [

	^self class name
]

{ #category : 'public-synchronization' }
RsrService >> synchronize [

	remoteSelf == nil
		ifFalse: [remoteSelf _synchronize]
]
