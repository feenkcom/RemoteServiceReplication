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
