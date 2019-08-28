"
I maintain the associations between locally stored objects and their remote counterparts.
"
Class {
	#name : #RsrRegistry,
	#superclass : #RsrObject,
	#instVars : [
		'mutex',
		'map',
		'storage',
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
RsrRegistry >> at: anOID [

	^self
		at: anOID
		ifAbsent: [self error: 'Unknown OID']
]

{ #category : #initialization }
RsrRegistry >> at: anOID
ifAbsent: aBlock [

	| element |
	element := self
		critical:
			[map
				at: anOID
				ifAbsent: [nil]].
	^element
		ifNil: aBlock
		ifNotNil: [element key]
]

{ #category : #protecting }
RsrRegistry >> critical: aBlock [

	mutex wait.
	^aBlock ensure: [mutex signal]
]

{ #category : #initialization }
RsrRegistry >> includes: aService [

	^self critical: [map includesKey: aService _id]
]

{ #category : #initialization }
RsrRegistry >> initialize [

	super initialize.
	mutex := Semaphore forMutualExclusion.
	map := Dictionary new.
	storage := IdentitySet new
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
RsrRegistry >> register: aService [

	| element |
	element := RsrWeakRegistryElement
		with: aService
		mournAction:
			[self removeKey: aService _id.
			reapAction value: aService _id].
	self critical: [map at: aService _id put: element]
]

{ #category : #'adding/removing' }
RsrRegistry >> removeKey: anRsrId [

	| element |
	element := self
		critical:
			[map
				removeKey: anRsrId
				ifAbsent: [nil]].
	^element ifNotNil: [element key]
]

{ #category : #initialization }
RsrRegistry >> retain: aService [
	"Keep a strong reference to the provided object."

	| element |
	element := RsrRegistryElement with: aService.
	self critical: [map at: aService _id put: element]
]
