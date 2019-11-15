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

	aCollection do: [:each | self analyze: each].
	^aCollection
]

{ #category : #analysis }
RsrRetainAnalysis >> analyzeDictionary: aDictionary [

	aDictionary
		keysAndValuesDo:
			[:key :value |
			self
				analyze: key;
				analyze: value].
	^aDictionary
]

{ #category : #analysis }
RsrRetainAnalysis >> analyzeImmediate: anObject [
	"Nothing to do for a generic immediate"

	^anObject
]

{ #category : #analysis }
RsrRetainAnalysis >> analyzeService: aService [

	self
		analyzing: aService
		during: [aService _reflectedVariablesDo: [:each | self analyze: each]]
]

{ #category : #analysis }
RsrRetainAnalysis >> analyzing: anObject
during: aBlock [

	(inFlight includes: anObject)
		ifTrue: [^RsrCycleDetected signal: 'Cycled detected on: ', anObject printString].
	inFlight add: anObject.
	self ensureRegistered: anObject.
	aBlock value.
	self retain: anObject.
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

{ #category : #testing }
RsrRetainAnalysis >> isCollection: anObject [

	^anObject isKindOf: Collection
]

{ #category : #testing }
RsrRetainAnalysis >> isDictionary: anObject [

	^anObject class == Dictionary
]

{ #category : #testing }
RsrRetainAnalysis >> isImmediate: anObject [

	^self encoder isImmediate: anObject
]

{ #category : #testing }
RsrRetainAnalysis >> isService: anObject [

	^anObject isKindOf: RsrService
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
RsrRetainAnalysis >> retain: anRsrObject [

	| retainCommand |
	retainCommand := RsrRetainObject object: anRsrObject.
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
