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
		ifFalse:
			[anRsrObject
				_id: self nextOid;
				_connection: self connection.
			anRsrObject registerWith: self registry]
]

{ #category : #initialization }
RsrRetainAnalysis >> initialize [

	super initialize.
	retainCommands := OrderedCollection new.
	inFlight := IdentitySet new
]

{ #category : #testing }
RsrRetainAnalysis >> isCollection: anObject [

	^anObject isCollection
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

	roots do: [:each | self process: each]
]

{ #category : #processing }
RsrRetainAnalysis >> process: anObject [

	(self isService: anObject)
		ifTrue: [^self processService: anObject].
	(self isCollection: anObject)
		ifTrue: [^self processCollection: anObject].
	(self isImmediate: anObject)
		ifTrue: [^anObject].
	^RsrUnsupportedObject signal: 'Unsupported object (' , anObject printString , ')'
]

{ #category : #processing }
RsrRetainAnalysis >> processCollection: aCollection [

	(self isDictionary: aCollection)
		ifTrue: [^self processDictionary: aCollection].
	aCollection do: [:each | self process: each].
	^aCollection
]

{ #category : #processing }
RsrRetainAnalysis >> processDictionary: aDictionary [

	aDictionary
		keysAndValuesDo:
			[:key :value |
			self
				process: key;
				process: value].
	^aDictionary
]

{ #category : #processing }
RsrRetainAnalysis >> processService: aService [

	self
		processing: aService
		during:
			[RsrReflection
				reflectedVariablesOf: aService
				do: [:each | self process: each]]
]

{ #category : #processing }
RsrRetainAnalysis >> processing: anObject
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
