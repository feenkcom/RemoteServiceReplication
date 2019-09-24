Class {
	#name : #RsrDictionarySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrDictionarySpecies class >> encode: aDictionary
with: anEncoder
onto: aStream [

	^anEncoder
		encodeDictionary: aDictionary
		on: aStream
]
