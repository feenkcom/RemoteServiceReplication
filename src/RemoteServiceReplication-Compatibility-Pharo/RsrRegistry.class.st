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
RsrRegistry >> at: anOID [

	^self at: anOID ifAbsent: [KeyNotFound signal: anOID asString]
]

{ #category : #initialization }
RsrRegistry >> at: anOID
ifAbsent: aBlock [

	| element |
	element := mutex critical: [map at: anOID ifAbsent: []].
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
				to: reaper
				with: aKey.
			RsrWeakRegistryElement value: aService].
	mutex critical: [map at: aKey put: element]
]

{ #category : #initialization }
RsrRegistry >> includes: anRsrObject [

	^mutex critical: [map includesKey: anRsrObject _id]
]

{ #category : #initialization }
RsrRegistry >> initialize [

	super initialize.
	map := Dictionary new.
	mutex := Semaphore forMutualExclusion.
	reaper := RsrObjectReaper registry: self
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

{ #category : #initialization }
RsrRegistry >> register: anRsrObject [
	"Keep a weak reference to the provided object.
	The simple #add: may be an better interface."

	self
		at: anRsrObject _id
		put: anRsrObject
]

{ #category : #'adding/removing' }
RsrRegistry >> removeKey: anRsrId [

	| element |
	element := mutex critical: [map removeKey: anRsrId ifAbsent: [nil]].
	^element ifNotNil: [element value]
]

{ #category : #initialization }
RsrRegistry >> retain: anRsrObject [
	"Keep a strong reference to the provided object."

	self
		at: anRsrObject _id
		put: anRsrObject
]
