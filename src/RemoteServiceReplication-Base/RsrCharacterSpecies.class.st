Class {
	#name : #RsrCharacterSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrCharacterSpecies class >> encodeReference: aCharacter
with: anEncoder
onto: aStream [

	^anEncoder
		encodeCharacter: aCharacter
		onto: aStream
]

{ #category : #accessing }
RsrCharacterSpecies class >> speciesIdentifier [

	^5
]
