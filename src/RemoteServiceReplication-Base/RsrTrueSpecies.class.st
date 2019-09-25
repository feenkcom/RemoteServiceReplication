Class {
	#name : #RsrTrueSpecies,
	#superclass : #RsrBooleanSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrTrueSpecies class >> decodeReference: aStream
for: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	^true
]

{ #category : #accessing }
RsrTrueSpecies class >> speciesIdentifier [

	^7
]
