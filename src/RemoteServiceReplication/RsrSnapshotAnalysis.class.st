"
No class-specific documentation for RsrSnapshotAnalysis, hierarchy is:
Object
  RsrObject
    RsrSnapshotAnalysis( roots snapshots inFlight connection)

"
Class {
	#name : 'RsrSnapshotAnalysis',
	#superclass : 'RsrObject',
	#instVars : [
		'roots',
		'snapshots',
		'inFlight',
		'connection'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrSnapshotAnalysis class >> roots: anArray
connection: aConnection [

	^self new
		roots: anArray;
		connection: aConnection;
		yourself
]

{ #category : 'analyzing' }
RsrSnapshotAnalysis >> analyze: anObject [

	^(self referenceClassFor: anObject)
		analyze: anObject
		using: self
]

{ #category : 'analyzing' }
RsrSnapshotAnalysis >> analyzeCollection: aCollection [

	self
		analyzing: aCollection
		during: [aCollection do: [:each | self analyze: each]].
	^aCollection
]

{ #category : 'analyzing' }
RsrSnapshotAnalysis >> analyzeDictionary: aDictionary [

	self
		analyzing: aDictionary
		during:
			[aDictionary
				keysAndValuesDo:
					[:key :value |
					self
						analyze: key;
						analyze: value]].
	^aDictionary
]

{ #category : 'analyzing' }
RsrSnapshotAnalysis >> analyzeImmediate: anImmediateObject [

	^anImmediateObject
]

{ #category : 'analyzing' }
RsrSnapshotAnalysis >> analyzeService: aService [

	self ensureRegistered: aService.
	self
		analyzing: aService
		during:
			[RsrServiceSnapshot
				reflectedVariablesFor: aService
				do: [:each | self analyze: each]].
	snapshots add: (RsrServiceSnapshot from: aService)
]

{ #category : 'analyzing' }
RsrSnapshotAnalysis >> analyzing: anObject
during: aBlock [

	(inFlight includes: anObject)
		ifTrue: [^RsrCycleDetected signal: anObject].
	inFlight add: anObject.
	aBlock value.
	inFlight remove: anObject
]

{ #category : 'accessing' }
RsrSnapshotAnalysis >> connection [

	^connection
]

{ #category : 'accessing' }
RsrSnapshotAnalysis >> connection: aConnection [

	connection := aConnection
]

{ #category : 'actions' }
RsrSnapshotAnalysis >> ensureRegistered: aService [

	self connection _ensureRegistered: aService.
	aService isServer
		ifTrue: [self connection _stronglyRetain: aService]
]

{ #category : 'initialization' }
RsrSnapshotAnalysis >> initialize [

	super initialize.
	snapshots := OrderedCollection new.
	inFlight := IdentitySet new
]

{ #category : 'actions' }
RsrSnapshotAnalysis >> perform [

	roots do: [:each | self analyze: each]
]

{ #category : 'accessing' }
RsrSnapshotAnalysis >> referenceClassFor: anObject [

	^RsrReference referenceClassFor: anObject
]

{ #category : 'accessing' }
RsrSnapshotAnalysis >> roots [

	^roots
]

{ #category : 'accessing' }
RsrSnapshotAnalysis >> roots: anArray [

	roots := anArray
]

{ #category : 'actions' }
RsrSnapshotAnalysis >> snapshot: aService [

	snapshots add: (RsrServiceSnapshot from: aService)
]

{ #category : 'accessing' }
RsrSnapshotAnalysis >> snapshots [

	^snapshots
]

{ #category : 'accessing' }
RsrSnapshotAnalysis >> snapshots: anOrderedCollection [

	snapshots := anOrderedCollection
]
