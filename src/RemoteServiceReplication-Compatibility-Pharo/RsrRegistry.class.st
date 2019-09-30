"
I maintain the associations between locally stored objects and their remote counterparts.
"
Class {
	#name : #RsrRegistry,
	#superclass : #RsrObject,
	#instVars : [
		'mutex',
		'map',
		'reapAction'
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
RsrRegistry >> at: aKey [

	^self at: aKey ifAbsent: [KeyNotFound signal: aKey asString]
]

{ #category : #initialization }
RsrRegistry >> at: aKey
ifAbsent: aBlock [

	| element |
	element := mutex critical: [map at: aKey ifAbsent: []].
	^element
		ifNil: aBlock
		ifNotNil: [element value]
]

{ #category : #initialization }
RsrRegistry >> at: aKey
put: aService [
	"Store aService into the registry"

	| element |
	element := aService isServer
		ifTrue: [RsrRegistryElement value: aService]
		ifFalse:
			[aService
				toFinalizeSend: #reap:
				to: self
				with: aKey.
			RsrWeakRegistryElement value: aService].
	mutex critical: [map at: aKey put: element]
]

{ #category : #hack }
RsrRegistry >> clear [

	map := Dictionary new
]

{ #category : #initialization }
RsrRegistry >> includesKey: aKey [

	^mutex critical: [map includesKey: aKey]
]

{ #category : #initialization }
RsrRegistry >> initialize [

	super initialize.
	map := Dictionary new.
	mutex := Semaphore forMutualExclusion
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

{ #category : #'adding/removing' }
RsrRegistry >> removeKey: aKey [

	| element |
	element := mutex critical: [map removeKey: aKey ifAbsent: [nil]].
	^element ifNotNil: [element value]
]
