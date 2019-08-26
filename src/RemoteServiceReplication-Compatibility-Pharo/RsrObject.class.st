Class {
	#name : #RsrObject,
	#superclass : #Object,
	#category : #'RemoteServiceReplication-Compatibility-Pharo-RemoteServiceReplication-Compatibility-Pharo'
}

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
RsrObject >> isDateTime: anObject [

	^anObject isKindOf: RsrDateTimeInterface dateTimeClass
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
