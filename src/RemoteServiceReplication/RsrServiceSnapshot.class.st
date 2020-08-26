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
	serviceName := aDecoder decodeAndResolveObjectReference: aStream.
	templateClass := (RsrClassResolver classNamed: serviceName) templateClass.
	template := templateClass templateClassName.
	targetServiceType := templateClass clientClassName = serviceName
		ifTrue: [#client]
		ifFalse: [#server].
	slots := OrderedCollection new: instVarCount.
	instVarCount timesRepeat: [slots add: (aDecoder decodeReference: aStream)]
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
	(RsrServiceSpecies reflectedVariablesFor: instance) size = slots size
		ifFalse: [self error: 'Incorrected encoded instance detected'].
	referenceStream := ReadStream on: slots.
	RsrServiceSpecies
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
