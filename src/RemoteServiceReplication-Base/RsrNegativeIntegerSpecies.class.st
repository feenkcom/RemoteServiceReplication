Class {
	#name : #RsrNegativeIntegerSpecies,
	#superclass : #RsrIntegerSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrNegativeIntegerSpecies class >> decodeWith: aDecoder
from: aStream [
	"Decode the provided bytes into the default native class for this species"

	^(RsrPositiveIntegerSpecies decodeWith: aDecoder from: aStream) negated
]

{ #category : #accessing }
RsrNegativeIntegerSpecies class >> speciesIdentifier [

	^4
]
