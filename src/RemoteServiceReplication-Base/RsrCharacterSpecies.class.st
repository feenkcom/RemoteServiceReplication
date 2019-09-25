Class {
	#name : #RsrCharacterSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

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
