Extension { #name : #RsrSpecies }

{ #category : #'*RemoteServiceReplication-Compatibility-GemStone' }
RsrSpecies class >> initializeSpeciesMapping [

	speciesMapping := Dictionary new.
	speciesMapping
		at: Symbol
		put: RsrSymbolSpecies.
	speciesMapping
		at: DoubleByteSymbol
		put: RsrSymbolSpecies.
	speciesMapping
		at: QuadByteSymbol
		put: RsrSymbolSpecies.
	speciesMapping
		at: String
		put: RsrStringSpecies.
    speciesMapping
        at: Unicode7
        put: RsrStringSpecies.
    speciesMapping
        at: DoubleByteString
        put: RsrStringSpecies.
    speciesMapping
        at: QuadByteString
        put: RsrStringSpecies.
	speciesMapping
		at: LargeInteger
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
		at: Boolean
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
	speciesMapping
		at: SmallDateAndTime
		put: RsrDateAndTimeSpecies.
	^speciesMapping
]
