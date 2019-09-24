Class {
	#name : #RsrObject,
	#superclass : #Object,
	#classInstVars : [
		'nativeClasses',
		'speciesMapping'
	],
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #initialization }
RsrObject class >> initializeNativeClasses [

	nativeClasses := Set new.
	nativeClasses
		add: ByteSymbol;
		add: WideSymbol;
		add: ByteString;
		add: WideString;
		add: LargePositiveInteger;
		add: LargeNegativeInteger;
		add: SmallInteger;
		add: Character;
		add: UndefinedObject;
		add: True;
		add: False;
		add: Array;
		add: ByteArray;
		add: Set;
		add: OrderedCollection;
		add: Dictionary;
		add: DateAndTime.
	^nativeClasses
]

{ #category : #initialization }
RsrObject class >> initializeSpeciesMapping [

	speciesMapping := Dictionary new.
	speciesMapping
		at: ByteSymbol
		put: RsrSymbolSpecies.
	speciesMapping
		at: WideSymbol
		put: RsrSymbolSpecies.
	speciesMapping
		at: ByteString
		put: RsrStringSpecies.
	speciesMapping
		at: WideString
		put: RsrStringSpecies.
	speciesMapping
		at: LargePositiveInteger
		put: RsrIntegerSpecies.
	speciesMapping
		at: LargeNegativeInteger
		put: RsrIntegerSpecies.
	speciesMapping
		at: SmallInteger
		put: RsrIntegerSpecies.
	speciesMapping
		at: Character
		put: RsrCharacterSpecies.
	speciesMapping
		at: UndefinedObject
		put: RsrUndefinedObjectSpecies.
	speciesMapping
		at: True
		put: RsrBooleanSpecies.
	speciesMapping
		at: False
		put: RsrBooleanSpecies.
	speciesMapping
		at: Array
		put: RsrArraySpecies.
	speciesMapping
		at: ByteArray
		put: RsrByteArraySpecies.
	speciesMapping
		at: Set
		put: RsrSetSpecies.
	speciesMapping
		at: OrderedCollection
		put: RsrOrderedCollectionSpecies.
	speciesMapping
		at: Dictionary
		put: RsrDictionarySpecies.
	speciesMapping
		at: DateAndTime
		put: RsrDateAndTimeSpecies.
	^speciesMapping
]

{ #category : #accessing }
RsrObject class >> nativeClasses [
	"Return the collection of classes supported by RSR in this environment"

	^nativeClasses ifNil: [self initializeNativeClasses]
]

{ #category : #'instance creation' }
RsrObject class >> new [

	^super new
]

{ #category : #accessing }
RsrObject class >> speciesMappings [
	"Return a mapping between the native class and their associated RsrSpecies"

	^speciesMapping ifNil: [self initializeSpeciesMapping]
]

{ #category : #initialization }
RsrObject >> initialize [

	^self
]

{ #category : #testing }
RsrObject >> isArray: anObject [

	^anObject isArray
]

{ #category : #testing }
RsrObject >> isByteArray: anObject [

	^anObject isKindOf: ByteArray
]

{ #category : #testing }
RsrObject >> isCharacter: anObject [

	^anObject isCharacter
]

{ #category : #testing }
RsrObject >> isCollection: anObject [

	^anObject isCollection
]

{ #category : #testing }
RsrObject >> isDateAndTime: anObject [

	^anObject isKindOf: RsrDateAndTimeInterface dateAndTimeClass
]

{ #category : #testing }
RsrObject >> isDictionary: anObject [

	^anObject isDictionary
]

{ #category : #testing }
RsrObject >> isInteger: anObject [

	^anObject isInteger
]

{ #category : #testing }
RsrObject >> isOrderedCollection: anObject [

	^anObject isKindOf: OrderedCollection
]

{ #category : #testing }
RsrObject >> isSet: anObject [

	^anObject isKindOf: Set
]

{ #category : #testing }
RsrObject >> isString: anObject [

	^anObject isString
]

{ #category : #testing }
RsrObject >> isSymbol: anObject [

	^anObject isSymbol
]

{ #category : #accessing }
RsrObject >> nativeClasses [
	"Return the collection of classes supported by RSR in this environment"

	^self class nativeClasses
]

{ #category : #accessing }
RsrObject >> speciesMappings [
	"Return a mapping between a native class and its RsrSpecies."

	^self class speciesMappings
]
