Class {
	#name : #RsrNegativeIntegerSpecies,
	#superclass : #RsrIntegerSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrNegativeIntegerSpecies class >> decodeReference: aStream for: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	^ (RsrPositiveIntegerSpecies decodeReference: aStream for: aDecoder)
		negated
]

{ #category : #accessing }
RsrNegativeIntegerSpecies class >> speciesIdentifier [

	^4
]
