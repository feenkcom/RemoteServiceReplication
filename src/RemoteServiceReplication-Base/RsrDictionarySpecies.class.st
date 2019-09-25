Class {
	#name : #RsrDictionarySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrDictionarySpecies class >> encodeReference: aDictionary
with: anEncoder
onto: aStream [

	^anEncoder
		encodeDictionary: aDictionary
		onto: aStream
]

{ #category : #accessing }
RsrDictionarySpecies class >> speciesIdentifier [

	^13
]
