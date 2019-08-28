Class {
	#name : #RsrObject,
	#superclass : #Object,
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

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
