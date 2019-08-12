"
I maintain the associations between locally stored objects and their remote counterparts.
"
Class {
	#name : #RsrRegistry,
	#superclass : #Object,
	#instVars : [
		'mutex',
		'map',
		'storage',
		'reapAction'
	],
	#classInstVars : [
		'uniqueInstance',
		'objectOffset'
	],
	#category : #'RemoteServiceReplication-Compatibility'
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
		ifNotNil: [element object]
]

{ #category : #initialization }
RsrRegistry >> includes: anRsrObject [

	^mutex critical: [map includesKey: anRsrObject rsrId]
]

{ #category : #initialization }
RsrRegistry >> initialize [

	super initialize.
	map := Dictionary new.
	storage := IdentitySet new.
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

{ #category : #initialization }
RsrRegistry >> register: anRsrObject [
	"Keep a weak reference to the provided object.
	The simple #add: may be an better interface."

	| reaper |
	mutex
		critical:
			[map
				at: anRsrObject rsrId
				put: (RsrRegistryElement object: anRsrObject)].
	reaper := RsrObjectReaper registry: self.
	anRsrObject
		toFinalizeSend: #reap:
		to: reaper
		with: anRsrObject rsrId
]

{ #category : #'adding/removing' }
RsrRegistry >> removeKey: anRsrId [

	| value |
	mutex
		critical:
			[value := map
				removeKey: anRsrId
				ifAbsent: [^nil].
			storage remove: value object ifAbsent: []].
	^value
]

{ #category : #initialization }
RsrRegistry >> retain: anRsrObject [
	"Keep a strong reference to the provided object."

	mutex
		critical:
			[map
				at: anRsrObject rsrId
				put: (RsrRegistryElement object: anRsrObject).
			storage add: anRsrObject].
]
