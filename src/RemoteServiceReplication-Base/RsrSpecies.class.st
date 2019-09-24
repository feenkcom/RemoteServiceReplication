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
RsrSpecies class >> encode: anObject
with: anEncoder
onto: aStream [
	"Encode the native object using the provided encoder"

	self subclassResponsibility
]

{ #category : #accessing }
RsrSpecies class >> nullSpecies [

	^RsrNullSpecies
]

{ #category : #accessing }
RsrSpecies class >> speciesMapping [
	"Return a mapping between the native class and their associated RsrSpecies"

	^speciesMapping ifNil: [self initializeSpeciesMapping]
]
