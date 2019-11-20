Class {
	#name : #RsrRetainAnalysis,
	#superclass : #RsrObject,
	#instVars : [
		'roots',
		'retainCommands',
		'inFlight',
		'connection'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrRetainAnalysis class >> roots: anArray
connection: aConnection [

	^self new
		roots: anArray;
		connection: aConnection;
		yourself
]

{ #category : #analysis }
RsrRetainAnalysis >> analyze: anObject [

	^(self speciesOf: anObject)
		analyze: anObject
		using: self
]

{ #category : #analysis }
RsrRetainAnalysis >> analyzeCollection: aCollection [

	self
		analyzing: aCollection
		during: [	aCollection do: [:each | self analyze: each]].
	^aCollection
]

{ #category : #analysis }
RsrRetainAnalysis >> analyzeDictionary: aDictionary [

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

{ #category : #analysis }
RsrRetainAnalysis >> analyzeImmediate: anObject [
	"Nothing to do for a generic immediate"

	^anObject
]

{ #category : #analysis }
RsrRetainAnalysis >> analyzeService: aService [

	self ensureRegistered: aService.
	self
		analyzing: aService
		during: [aService _reflectedVariablesDo: [:each | self analyze: each]].
	self retain: aService
]

{ #category : #analysis }
RsrRetainAnalysis >> analyzing: anObject
during: aBlock [

	(inFlight includes: anObject)
		ifTrue: [^RsrCycleDetected signal: anObject].
	inFlight add: anObject.
	aBlock value.
	inFlight remove: anObject
]

{ #category : #accessing }
RsrRetainAnalysis >> connection [

	^connection
]

{ #category : #accessing }
RsrRetainAnalysis >> connection: aConnection [

	connection := aConnection
]

{ #category : #accessing }
RsrRetainAnalysis >> encoder [

	^self connection encoder
]

{ #category : #actions }
RsrRetainAnalysis >> ensureRegistered: anRsrObject [

	anRsrObject isMirrored
		ifTrue: [^self].
	anRsrObject
		_id: self nextOid
		connection: self connection.
	anRsrObject _addTo: self registry
]

{ #category : #initialization }
RsrRetainAnalysis >> initialize [

	super initialize.
	retainCommands := OrderedCollection new.
	inFlight := IdentitySet new
]

{ #category : #accessing }
RsrRetainAnalysis >> nextOid [

	^self connection oidSpigot next
]

{ #category : #actions }
RsrRetainAnalysis >> perform [

	roots do: [:each | self analyze: each]
]

{ #category : #accessing }
RsrRetainAnalysis >> registry [

	^connection registry
]

{ #category : #actions }
RsrRetainAnalysis >> retain: aService [

	| retainCommand |
	retainCommand := RsrRetainObject object: aService.
	retainCommand encodeUsing: self encoder.
	self retainCommands add: retainCommand
]

{ #category : #actions }
RsrRetainAnalysis >> retainCommands [

	^retainCommands
]

{ #category : #accessing }
RsrRetainAnalysis >> roots [

	^roots
]

{ #category : #accessing }
RsrRetainAnalysis >> roots: anObject [

	roots := anObject
]

{ #category : #accessing }
RsrRetainAnalysis >> speciesOf: anObject [

	^self encoder speciesOf: anObject
]
