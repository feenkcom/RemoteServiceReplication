Class {
	#name : #RsrObject,
	#superclass : #Object,
	#classInstVars : [
		'nativeClasses'
	],
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #accessing }
RsrObject class >> nativeClasses [
	"Return the collection of classes supported by RSR in this environment"

	^Set withAll: RsrSpecies speciesMapping keys
]

{ #category : #'instance creation' }
RsrObject class >> new [

	^super new
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
