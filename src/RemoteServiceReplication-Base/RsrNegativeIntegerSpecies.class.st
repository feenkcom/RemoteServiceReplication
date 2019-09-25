Class {
	#name : #RsrNegativeIntegerSpecies,
	#superclass : #RsrIntegerSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrNegativeIntegerSpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	^(RsrPositiveIntegerSpecies decodeReference: aStream using: aDecoder) negated
]

{ #category : #accessing }
RsrNegativeIntegerSpecies class >> speciesIdentifier [

	^4
]
