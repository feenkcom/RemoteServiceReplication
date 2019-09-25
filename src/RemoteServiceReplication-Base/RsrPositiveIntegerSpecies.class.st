Class {
	#name : #RsrPositiveIntegerSpecies,
	#superclass : #RsrIntegerSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrPositiveIntegerSpecies class >> decodeReference: aStream
for: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	| length bytes |
	length := aDecoder decodeControlWord: aStream.
	bytes := aStream next: length.
	^aDecoder bytesAsInteger: bytes
]

{ #category : #accessing }
RsrPositiveIntegerSpecies class >> speciesIdentifier [

	^3
]
