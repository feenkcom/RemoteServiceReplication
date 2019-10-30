!RsrSpecies class methodsFor!
initializeSpeciesMapping

	speciesMapping := Dictionary new.
	speciesMapping
		at: Symbol
		put: RsrSymbolSpecies.
	speciesMapping
		at: String
		put: RsrStringSpecies.
	speciesMapping
		at: Utf8String
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
		at: TimeStamp
		put: RsrDateAndTimeSpecies.
	^speciesMapping! !


!RsrCharacterArraySpecies class methodsFor!
fromBytes: aByteArray

	^Utf8String fromByteArray: aByteArray!

toBytes: aCharacterArray

	^aCharacterArray asUtf8String asByteArray! !


!RsrDateAndTimeSpecies class methodsFor!
fromMicroseconds: anInteger

	^TimeStamp fromMilliseconds: self posixEpoch asMilliseconds + (anInteger // 1000)!

microsecondsSinceEpoch: aTimeStamp

	| millisDiff |
	millisDiff := aTimeStamp asMilliseconds - self posixEpoch asMilliseconds.
	^millisDiff * 1000!

now

	^TimeStamp current!

posixEpoch

	^TimeStamp fromSeconds: 2177452800! !
