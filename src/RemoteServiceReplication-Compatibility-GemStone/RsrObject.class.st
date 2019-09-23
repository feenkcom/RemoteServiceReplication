Class {
	#name : #RsrObject,
	#superclass : #Object,
	#classInstVars : ['nativeClasses'],
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #accessing }
RsrObject class >> nativeClasses [
	"Return the collection of classes supported by RSR in this environment"

	^nativeClasses ifNil: [self initializeNativeClasses]
]

{ #category : #initialization }
RsrObject class >> initializeNativeClasses [

	nativeClasses := Set new.
	nativeClasses
		add: Symbol;
		add: String;
		add: LargeInteger;
		add: SmallInteger;
		add: Character;
		add: UndefinedObject;
		add: Boolean;
		add: Array;
		add: ByteArray;
		add: Set;
		add: OrderedCollection;
		add: Dictionary;
		add: DateAndTime.
	^nativeClasses
]

{ #category : #'instance creation' }
RsrObject class >> new [

	^super new initialize
]

{ #category : #initialization }
RsrObject >> initialize [

	^self
]

{ #category : #testing }
RsrObject >> isArray: anObject [

	^anObject isKindOf: Array
]

{ #category : #testing }
RsrObject >> isByteArray: anObject [

	^anObject isKindOf: ByteArray
]

{ #category : #testing }
RsrObject >> isCharacter: anObject [

	^anObject isKindOf: Character
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

	^anObject isKindOf: Dictionary
]

{ #category : #testing }
RsrObject >> isInteger: anObject [

	^anObject isKindOf: Integer
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