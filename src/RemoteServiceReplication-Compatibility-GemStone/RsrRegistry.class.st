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
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
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

{ #category : #accessing }
RsrRegistry >> at: aKey
put: anEntry [

	(anEntry isKindOf: RsrService)
		ifTrue: [self error].
	mutex critical: [map at: aKey put: anEntry].
	^anEntry
]

{ #category : #accessing }
RsrRegistry >> dispatcherAt: aKey [

	^self
		dispatcherAt: aKey
		ifAbsent: [self error: 'Unknown key(', aKey printString, ')']
]

{ #category : #accessing }
RsrRegistry >> dispatcherAt: aKey
ifAbsent: aBlock [

	| element |
	element := mutex critical: [map at: aKey ifAbsent: []].
	^element
		ifNil: aBlock
		ifNotNil: [element dispatcher]
]

{ #category : #initialization }
RsrRegistry >> includesKey: aKey [

	^mutex critical: [map includesKey: aKey]
]

{ #category : #initialization }
RsrRegistry >> initialize [

	super initialize.
	mutex := Semaphore forMutualExclusion.
	map := Dictionary new
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
RsrRegistry >> removeKey: anRsrId [

	| element |
	element := mutex
		critical:
			[map
				removeKey: anRsrId
				ifAbsent: [nil]].
	^element ifNotNil: [element value]
]

{ #category : #'accessing' }
RsrRegistry >> serviceAt: aKey [

	^self
		serviceAt: aKey
		ifAbsent: [self error: 'Unknown key(', aKey printString, ')']
]

{ #category : #'accessing' }
RsrRegistry >> serviceAt: aKey
ifAbsent: aBlock [

	| element |
	element := mutex critical: [map at: aKey ifAbsent: []].
	^element
		ifNil: aBlock
		ifNotNil: [element service]
]

{ #category : #'adding/removing' }
RsrRegistry >> serviceAt: aKey
put: aService [
	"Store aService into the registry"

	| dispatcher entry |
	dispatcher := (RsrClassResolver classNamed: #RsrMessageDispatcher) startOn: aService _connection.
	entry := aService isServer
			ifTrue: [RsrRegistryEntry with: aService]
			ifFalse:
					[RsrWeakRegistryEntry
							with: aService
							mournAction:
									[self removeKey: aKey.
									reapAction value: aKey]].
	entry dispatcher: dispatcher.
	mutex critical: [map at: aKey put: entry].
	^aService
]
