"
I maintain the associations between locally stored objects and their remote counterparts.
"
Class {
	#name : 'RsrRegistry',
	#superclass : 'RsrObject',
	#instVars : [
		'mutex',
		'map',
		'reapAction'
	],
	#category : 'RemoteServiceReplication-GemStone'
}

{ #category : 'instance creation' }
RsrRegistry class >> new [

	^self reapAction: [:key |]
]

{ #category : 'instance creation' }
RsrRegistry class >> reapAction: aBlock [

	^super new
		reapAction: aBlock;
		yourself
]

{ #category : 'accessing' }
RsrRegistry >> _At: aKey
ifAbsent: aBlock [
	"Temporarily return the Registry's entry so we can properly maintain the lifetime."

	^mutex critical: [map at: aKey ifAbsent: aBlock]
]

{ #category : 'adding/removing' }
RsrRegistry >> cleanupEntryFor: aKey [

	self removeKey: aKey
]

{ #category : 'initialization' }
RsrRegistry >> includesKey: aKey [

	^mutex critical: [map includesKey: aKey]
]

{ #category : 'initialization' }
RsrRegistry >> initialize [

	super initialize.
	mutex := Semaphore forMutualExclusion.
	map := Dictionary new
]

{ #category : 'accessing' }
RsrRegistry >> reapAction [

	^reapAction
]

{ #category : 'accessing' }
RsrRegistry >> reapAction: aBlock [

	reapAction := aBlock
]

{ #category : 'adding/removing' }
RsrRegistry >> removeKey: anRsrId [

	| entry |
	entry := mutex
		critical:
			[map
				removeKey: anRsrId
				ifAbsent: [nil]].
	^entry
]

{ #category : 'accessing' }
RsrRegistry >> serviceAt: aSID [

	^self
		serviceAt: aSID
		ifAbsent: [RsrUnknownOID signal: aSID printString]
]

{ #category : 'accessing' }
RsrRegistry >> serviceAt: aKey
ifAbsent: aBlock [

	| element |
	element := mutex critical: [map at: aKey ifAbsent: []].
	^element
		ifNil: aBlock
		ifNotNil: [element service]
]

{ #category : 'accessing' }
RsrRegistry >> serviceAt: aKey
put: aService [
	"Store aService into the registry"

	| mournAction entry |
	mournAction := aService isServer
		ifTrue: [[self cleanupEntryFor: aKey]]
		ifFalse:
			[[self cleanupEntryFor: aKey.
			reapAction value: aKey]].
	entry := RsrRegistryEntry
		service: aService
		onMourn: mournAction.
	aService isServer
		ifTrue: [entry becomeStrong].
	mutex critical: [map at: aKey put: entry].
	^aService
]
