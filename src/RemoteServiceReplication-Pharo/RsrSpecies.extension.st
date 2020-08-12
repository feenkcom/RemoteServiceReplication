Extension { #name : 'RsrSpecies' }

{ #category : '*RemoteServiceReplication-Pharo' }
RsrSpecies class >> initializeSpeciesMapping [

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
