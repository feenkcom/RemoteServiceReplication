Class {
	#name : #RsrOrderedCollectionSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrOrderedCollectionSpecies class >> encodeReference: anOrderedCollection
with: anEncoder
onto: aStream [

	^anEncoder
		encodeOrderedCollection: anOrderedCollection
		on: aStream
]

{ #category : #accessing }
RsrOrderedCollectionSpecies class >> speciesIdentifier [

	^12
]
