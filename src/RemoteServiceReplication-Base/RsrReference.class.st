Class {
	#name : 'RsrReference',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'instance creation' }
RsrReference class >> from: anObject [

	| referenceClass |
	referenceClass := self referenceClassFor: anObject.
	^referenceClass from: anObject
]

{ #category : 'private-accessing' }
RsrReference class >> referenceClassFor: anObject [

	(anObject isKindOf: RsrService)
		ifTrue: [^RsrServiceReference].
	^self referenceMapping
		at: anObject class
		ifAbsent: [RsrUnsupportedObject signal: anObject]
]

{ #category : 'private-accessing' }
RsrReference class >> referenceMapping [

	| referenceMapping |
	referenceMapping := Dictionary new.
	referenceMapping
		at: Symbol
		put: RsrSymbolReference.
	referenceMapping
		at: DoubleByteSymbol
		put: RsrSymbolReference.
	referenceMapping
		at: QuadByteSymbol
		put: RsrSymbolReference.
	referenceMapping
		at: String
		put: RsrStringReference.
    referenceMapping
        at: Unicode7
        put: RsrStringReference.
    referenceMapping
        at: DoubleByteString
        put: RsrStringReference.
    referenceMapping
        at: QuadByteString
        put: RsrStringReference.
	referenceMapping
		at: LargeInteger
		put: RsrIntegerReference.
	referenceMapping
		at: SmallInteger
		put: RsrIntegerReference.
	referenceMapping
		at: Character
		put: RsrCharacterReference.
	referenceMapping
		at: UndefinedObject
		put: RsrNilReference.
	referenceMapping
		at: Boolean
		put: RsrTrueReference.
	referenceMapping
		at: Array
		put: RsrArrayReference.
	referenceMapping
		at: ByteArray
		put: RsrByteArrayReference.
	referenceMapping
		at: Set
		put: RsrSetReference.
	referenceMapping
		at: OrderedCollection
		put: RsrOrderedCollectionReference.
	referenceMapping
		at: Dictionary
		put: RsrDictionaryReference.
	referenceMapping
		at: DateAndTime
		put: RsrDateAndTimeReference.
	referenceMapping
		at: SmallDateAndTime
		put: RsrDateAndTimeReference.
	^referenceMapping
]

{ #category : 'accessing' }
RsrReference class >> typeIdentifier [

	^self subclassResponsibility
]

{ #category : 'accessing' }
RsrReference >> typeIdentifier [

	^self class typeIdentifier
]
