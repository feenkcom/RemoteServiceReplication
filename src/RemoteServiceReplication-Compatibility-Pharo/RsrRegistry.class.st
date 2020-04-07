"
I maintain the associations between locally stored objects and their remote counterparts.
"
Class {
	#name : #RsrRegistry,
	#superclass : #RsrObject,
	#instVars : [
		'mutex',
		'map',
		'reapAction',
		'reaper'
	],
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #'instance creation' }
RsrRegistry class >> new [

	^self reapAction: [:key |]
]

{ #category : #'instance creation' }
RsrRegistry class >> reapAction: aBlock [

	^super new
		reapAction: aBlock;
		yourself
]

{ #category : #initialization }
RsrRegistry >> at: aKey
put: anEntry [
	"Store aService into the registry"

	(anEntry isKindOf: RsrService)
		ifTrue: [^self error].
	mutex critical: [map at: aKey put: anEntry]
]

{ #category : #initialization }
RsrRegistry >> includesKey: aKey [

	^mutex critical: [map includesKey: aKey]
]

{ #category : #initialization }
RsrRegistry >> initialize [

	super initialize.
	map := Dictionary new.
	mutex := Semaphore forMutualExclusion.
	reaper := RsrServiceReaper for: self
]

{ #category : #reaping }
RsrRegistry >> reap: aKey [

	self removeKey: aKey.
	self reapAction value: aKey
]

{ #category : #accessing }
RsrRegistry >> reapAction [

	^reapAction
]

{ #category : #accessing }
RsrRegistry >> reapAction: aBlock [

	reapAction := aBlock
]

{ #category : #accessing }
RsrRegistry >> reaper [

	^reaper
]

{ #category : #'adding/removing' }
RsrRegistry >> removeKey: aKey [

	| element |
	element := mutex critical: [map removeKey: aKey ifAbsent: [nil]].
	^element ifNotNil: [element value]
]

{ #category : #initialization }
RsrRegistry >> serviceAt: aKey [

	^self
		serviceAt: aKey
		ifAbsent: [KeyNotFound signal: aKey asString]
]

{ #category : #initialization }
RsrRegistry >> serviceAt: aKey
ifAbsent: aBlock [

	| element |
	element := mutex critical: [map at: aKey ifAbsent: []].
	^element
		ifNil: aBlock
		ifNotNil: [element service]
]

{ #category : #initialization }
RsrRegistry >> serviceAt: aKey
put: anEntry [
	"Store aService into the registry"

	(anEntry isKindOf: RsrService)
		ifTrue: [^anEntry _addTo: self]
]
