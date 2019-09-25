Class {
	#name : #RsrOrderedCollectionSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrOrderedCollectionSpecies class >> encodeReference: anOrderedCollection
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: anOrderedCollection size
		onto: aStream.
	anOrderedCollection
		do:
			[:each |
			anEncoder
				encodeReferenceOf: each
				onto: aStream]
]

{ #category : #accessing }
RsrOrderedCollectionSpecies class >> speciesIdentifier [

	^12
]
