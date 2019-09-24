Class {
	#name : #RsrCharacterSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrCharacterSpecies class >> encode: aCharacter
with: anEncoder
onto: aStream [

	^anEncoder
		encodeCharacter: aCharacter
		onto: aStream
]
