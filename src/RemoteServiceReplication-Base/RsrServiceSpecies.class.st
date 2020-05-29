Class {
	#name : 'RsrServiceSpecies',
	#superclass : 'RsrSpecies',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'object analysis' }
RsrServiceSpecies class >> analyze: aService
using: anAnalyzer [
	"A method that works in conjunction with RsrRetainAnalysis to analyze
	an object"

	^anAnalyzer analyzeService: aService
]

{ #category : 'coding' }
RsrServiceSpecies class >> decode: aStream
using: aDecoder [
	"Decode a Service from the stream"

	| species oid instVarCount serviceName instance |
	species := aDecoder decodeControlWord: aStream.
	oid := aDecoder decodeControlWord: aStream.
	instVarCount := aDecoder decodeControlWord: aStream.
	serviceName := aDecoder decodeObjectReference: aStream.
	instance := aDecoder registry
		serviceAt: oid
		ifAbsent:
			[((aDecoder lookupClass: serviceName)
				_id: oid
				connection: aDecoder connection)
					yourself].
	(aDecoder registry includesKey: oid)
		ifFalse:
			[aDecoder registry
				serviceAt: instance _id
				put: instance].
	(self reflectedVariablesFor: instance) size = instVarCount
		ifFalse: [self error: 'Incorrectly encoded instance detected'].
	self
		reflectedVariableIndicesFor: instance
		do: [:index | instance instVarAt: index put: (aDecoder decodeObjectReference: aStream)].
	^instance
]

{ #category : 'coding' }
RsrServiceSpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	self shouldNotImplement
]

{ #category : 'coding' }
RsrServiceSpecies class >> encode: aService
using: anEncoder
on: aStream [
	"Encode this object. This is specifically used by RsrServiceSpecies."

	"type"
	"the OID for the object"
	"the name of the remote service to create"
	"Write the object slots"
	| reflectedVariables remoteServiceName |
	reflectedVariables := self reflectedVariablesFor: aService.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: aService _id
		onto: aStream.
	anEncoder
		encodeControlWord: reflectedVariables size
		onto: aStream.
	remoteServiceName := aService isClient
		ifTrue: [aService class serverClassName]
		ifFalse: [aService class clientClassName].
	(anEncoder speciesOf: remoteServiceName)
		encodeReference: remoteServiceName
		using: anEncoder
		onto: aStream.
	RsrServiceSpecies
		reflectedVariablesFor: aService
		do: [:each | anEncoder encodeReferenceOf: each onto: aStream]
]

{ #category : 'coding' }
RsrServiceSpecies class >> encodeReference: aService
using: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: aService _id
		onto: aStream
]

{ #category : 'enumerating' }
RsrServiceSpecies class >> reflectedVariableIndicesFor: aService
do: aBlock [

	| allVariables |
	allVariables := aService class allInstVarNames.
	(self reflectedVariablesFor: aService)
		do:
			[:varName | | index |
			index := allVariables indexOf: varName.
			aBlock value: index]
]

{ #category : 'accessing' }
RsrServiceSpecies class >> reflectedVariablesFor: aService [

	| currentClass variables |
	variables := OrderedCollection new.
	currentClass := aService class abstractClass.
	[currentClass == RsrService]
		whileFalse:
			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].
			currentClass := currentClass superclass].
	^variables
]

{ #category : 'accessing' }
RsrServiceSpecies class >> reflectedVariablesFor: aService
do: aBlock [

	self
		reflectedVariableIndicesFor: aService
		do: [:index | aBlock value: (aService instVarAt: index)]
]

{ #category : 'accessing' }
RsrServiceSpecies class >> speciesIdentifier [

	^0
]
