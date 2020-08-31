"
No class-specific documentation for RsrServiceSnapshot, hierarchy is:
Object
  RsrObject
    RsrServiceSnapshot( sid template type slots)

"
Class {
	#name : 'RsrServiceSnapshot',
	#superclass : 'RsrObject',
	#instVars : [
		'sid',
		'template',
		'targetServiceType',
		'slots'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrServiceSnapshot class >> from: aService [

	^self new
		snapshot: aService;
		yourself
]

{ #category : 'variable utilites' }
RsrServiceSnapshot class >> reflectedVariableIndicesFor: aService
do: aBlock [

	| allVariables |
	allVariables := aService class allInstVarNames.
	(self reflectedVariablesFor: aService)
		do:
			[:varName | | index |
			index := allVariables indexOf: varName.
			aBlock value: index]
]

{ #category : 'variable utilites' }
RsrServiceSnapshot class >> reflectedVariablesFor: aService [

	| currentClass variables |
	variables := OrderedCollection new.
	currentClass := aService class templateClass.
	[currentClass == RsrService]
		whileFalse:
			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].
			currentClass := currentClass superclass].
	^variables
]

{ #category : 'variable utilites' }
RsrServiceSnapshot class >> reflectedVariablesFor: aService
do: aBlock [

	self
		reflectedVariableIndicesFor: aService
		do: [:index | aBlock value: (aService instVarAt: index)]
]

{ #category : 'accessing' }
RsrServiceSnapshot >> createBasicInstance [

	^self shouldCreateClient
		ifTrue: [self templateClass clientClass basicNew]
		ifFalse: [self templateClass serverClass basicNew]
]

{ #category : 'encoding/decoding' }
RsrServiceSnapshot >> decode: aStream
using: aDecoder [

	| species instVarCount serviceName templateClass |
	species := aDecoder decodeControlWord: aStream.
	sid := aDecoder decodeControlWord: aStream.
	instVarCount := aDecoder decodeControlWord: aStream.
	serviceName := (aDecoder decodeReference: aStream) resolve: aDecoder registry.
	templateClass := (RsrClassResolver classNamed: serviceName) templateClass.
	template := templateClass templateClassName.
	targetServiceType := templateClass clientClassName = serviceName
		ifTrue: [#client]
		ifFalse: [#server].
	slots := OrderedCollection new: instVarCount.
	instVarCount timesRepeat: [slots add: (aDecoder decodeReference: aStream)]
]

{ #category : 'encoding/decoding' }
RsrServiceSnapshot >> encode: aStream
using: anEncoder [

	anEncoder
		encodeControlWord: self snapshotIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: self sid
		onto: aStream.
	anEncoder
		encodeControlWord: self slots size
		onto: aStream.
	self targetClassNameReference
		encode: aStream
		using: anEncoder.
	self slots do: [:each | each encode: aStream using: anEncoder]
]

{ #category : 'accessing' }
RsrServiceSnapshot >> instanceIn: aConnection [

	^aConnection registry
		serviceAt: self sid
		ifAbsent:
			[| instance |
			instance := self createBasicInstance.
			instance
				_id: self sid
				connection: aConnection.
			aConnection registry
				serviceAt: self sid
				put: instance.
			instance]
]

{ #category : 'reifying' }
RsrServiceSnapshot >> reifyIn: aConnection [

	| instance referenceStream |
	instance := self instanceIn: aConnection.
	(self class reflectedVariablesFor: instance) size = slots size
		ifFalse: [self error: 'Incorrected encoded instance detected'].
	referenceStream := ReadStream on: slots.
	self class
		reflectedVariableIndicesFor: instance
		do: [:index | instance instVarAt: index put: (referenceStream next resolve: aConnection registry)].
	^instance
]

{ #category : 'testing' }
RsrServiceSnapshot >> shouldCreateClient [

	^self targetServiceType == #client
]

{ #category : 'accessing' }
RsrServiceSnapshot >> sid [

	^sid
]

{ #category : 'accessing' }
RsrServiceSnapshot >> sid: aServiceID [

	sid := aServiceID
]

{ #category : 'accessing' }
RsrServiceSnapshot >> slots [

	^slots
]

{ #category : 'accessing' }
RsrServiceSnapshot >> slots: anArrayOfReferences [

	slots := anArrayOfReferences
]

{ #category : 'snapshotting' }
RsrServiceSnapshot >> snapshot: aService [

	sid := aService _id.
	template := aService class templateClassName.
	targetServiceType := aService isClient
		ifTrue: [#server]
		ifFalse: [#client].
	slots := OrderedCollection new.
	RsrServiceSnapshot
		reflectedVariablesFor: aService
		do: [:each | slots add: (RsrReference from: each)]
]

{ #category : 'accessing' }
RsrServiceSnapshot >> snapshotIdentifier [

	^0
]

{ #category : 'accessing' }
RsrServiceSnapshot >> targetClassNameReference [

	| targetClassName |
	targetClassName := self shouldCreateClient
		ifTrue: [self templateClass clientClassName]
		ifFalse: [self templateClass serverClassName].
	^RsrSymbolReference symbol: targetClassName
]

{ #category : 'accessing' }
RsrServiceSnapshot >> targetServiceType [

	^targetServiceType
]

{ #category : 'accessing' }
RsrServiceSnapshot >> targetServiceType: aSymbol [

	targetServiceType := aSymbol
]

{ #category : 'accessing' }
RsrServiceSnapshot >> template [

	^template
]

{ #category : 'accessing' }
RsrServiceSnapshot >> template: aSymbol [

	template := aSymbol
]

{ #category : 'accessing' }
RsrServiceSnapshot >> templateClass [

	^RsrClassResolver classNamed: self template
]
