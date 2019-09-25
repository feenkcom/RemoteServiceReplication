Class {
	#name : #RsrSpecies,
	#superclass : #Object,
	#classInstVars : [
		'speciesMapping'
	],
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrSpecies class >> decode: bytes [
	"Decode the provided bytes into the default native class for this species"

	self notYetImplemented
]

{ #category : #coding }
RsrSpecies class >> decodeBody: aStream [
	"Decode the provided bytes into the default native class for this species"

	self notYetImplemented
]

{ #category : #coding }
RsrSpecies class >> decodeReference: aStream
for: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	| decodeSelector |
	decodeSelector := aDecoder decodeImmediateObjectMap at: self speciesIdentifier.
	^aDecoder
		perform: decodeSelector
		with: aStream
]

{ #category : #coding }
RsrSpecies class >> encodeReference: anObject
with: anEncoder
onto: aStream [
	"Encode the native object using the provided encoder"

	self subclassResponsibility
]

{ #category : #accessing }
RsrSpecies class >> immediateOID [

	^0
]

{ #category : #accessing }
RsrSpecies class >> nullSpecies [

	^RsrNullSpecies
]

{ #category : #accessing }
RsrSpecies class >> speciesIdentifier [

	^self subclassResponsibility
]

{ #category : #accessing }
RsrSpecies class >> speciesList [

	^{RsrServiceSpecies.
	RsrSymbolSpecies.
	RsrStringSpecies.
	RsrPositiveIntegerSpecies.
	RsrNegativeIntegerSpecies.
	RsrCharacterSpecies.
	RsrUndefinedObjectSpecies.
	RsrTrueSpecies.
	RsrFalseSpecies.
	RsrArraySpecies.
	RsrByteArraySpecies.
	RsrSetSpecies.
	RsrOrderedCollectionSpecies.
	RsrDictionarySpecies.
	RsrDateAndTimeSpecies.}
]

{ #category : #accessing }
RsrSpecies class >> speciesMapping [
	"Return a mapping between the native class and their associated RsrSpecies"

	^speciesMapping ifNil: [self initializeSpeciesMapping]
]
