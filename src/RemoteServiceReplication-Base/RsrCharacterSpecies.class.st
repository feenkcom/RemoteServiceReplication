Class {
	#name : #RsrCharacterSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrCharacterSpecies class >> decodeWith: aDecoder
from: aStream [
	"Decode the provided bytes into the default native class for this species"

	| codePoint |
	codePoint := aDecoder decodeControlWord: aStream.
	^Character codePoint: codePoint
]

{ #category : #coding }
RsrCharacterSpecies class >> encodeReference: aCharacter
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: aCharacter codePoint
		onto: aStream
]

{ #category : #accessing }
RsrCharacterSpecies class >> speciesIdentifier [

	^5
]
