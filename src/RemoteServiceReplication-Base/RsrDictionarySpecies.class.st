Class {
	#name : #RsrDictionarySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrDictionarySpecies class >> encodeReference: aDictionary
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: aDictionary size
		onto: aStream.
	aDictionary
		keysAndValuesDo:
			[:key :value |
			anEncoder
				encodeReferenceOf: key
				onto: aStream.
			anEncoder
				encodeReferenceOf: value
				onto: aStream]
]

{ #category : #accessing }
RsrDictionarySpecies class >> speciesIdentifier [

	^13
]
