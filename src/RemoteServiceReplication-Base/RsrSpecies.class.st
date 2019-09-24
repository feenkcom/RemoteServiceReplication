Class {
	#name : #RsrSpecies,
	#superclass : #Object,
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
