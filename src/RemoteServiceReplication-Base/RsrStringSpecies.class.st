Class {
	#name : #RsrStringSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrStringSpecies class >> encodeReference: aString
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeStringBody: aString
		onto: aStream
]

{ #category : #accessing }
RsrStringSpecies class >> speciesIdentifier [

	^2
]
