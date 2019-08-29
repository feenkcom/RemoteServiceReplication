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

{ #category : #initialization }
RsrRegistry >> at: aKey [

	^self
		at: aKey
		ifAbsent: [self error: 'Unknown key(', aKey printString, ')']
]

{ #category : #initialization }
RsrRegistry >> at: aKey
ifAbsent: aBlock [

	| element |
	element := self
		critical:
			[map
				at: aKey
				ifAbsent: [nil]].
	^element
		ifNil: aBlock
		ifNotNil: [element value]
]

{ #category : #accessing }
RsrRegistry >> at: aKey
put: aService [

	| element |
	element := aService isServer
		ifTrue: [RsrRegistryElement with: aService]
		ifFalse:
			[RsrWeakRegistryElement
				with: aService
				mournAction:
					[self removeKey: aKey.
					reapAction value: aKey]].
	self critical: [map at: aKey put: element]
]

{ #category : #protecting }
RsrRegistry >> critical: aBlock [

	mutex wait.
	^aBlock ensure: [mutex signal]
]

{ #category : #initialization }
RsrRegistry >> includesKey: aKey [

	^self critical: [map includesKey: aKey]
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
	element := self
		critical:
			[map
				removeKey: anRsrId
				ifAbsent: [nil]].
	^element ifNotNil: [element value]
]
