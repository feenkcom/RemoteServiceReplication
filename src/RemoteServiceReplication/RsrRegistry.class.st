"
I maintain the associations between locally stored objects and their remote counterparts.
"
Class {
	#name : #RsrRegistry,
	#superclass : #Object,
	#instVars : [
		'mutex',
		'map',
		'storage'
	],
	#classInstVars : [
		'uniqueInstance',
		'objectOffset'
	],
	#category : 'RemoteServiceReplication-Utilities'
}

{ #category : #initialization }
RsrRegistry >> at: anOID [

	^mutex critical: [map at: anOID]
]

{ #category : #initialization }
RsrRegistry >> at: anOID
ifAbsent: aBlock [

	^mutex
		critical:
			[map
				at: anOID
				ifAbsent: aBlock]
]

{ #category : #initialization }
RsrRegistry >> includes: anRsrObject [

	^mutex critical: [map includesKey: anRsrObject rsrId]
]

{ #category : #initialization }
RsrRegistry >> initialize [

	super initialize.
	map := WeakValueDictionary new.
	storage := IdentitySet new.
	mutex := Semaphore forMutualExclusion
]

{ #category : #initialization }
RsrRegistry >> register: anRsrObject [
	"Keep a weak reference to the provided object.
	The simple #add: may be an better interface."

	mutex
		critical:
			[map
				at: anRsrObject rsrId
				put: anRsrObject]
]

{ #category : #'adding/removing' }
RsrRegistry >> removeKey: anRsrId [

	| value |
	mutex
		critical:
			[value := map
				removeKey: anRsrId
				ifAbsent: [^nil].
			storage remove: value].
	^value
]

{ #category : #initialization }
RsrRegistry >> retain: anRsrObject [
	"Keep a strong reference to the provided object."

	mutex
		critical:
			[map
				at: anRsrObject rsrId
				put: anRsrObject.
			storage add: anRsrObject]
]
