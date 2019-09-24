Class {
	#name : #RsrOrderedCollectionSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrOrderedCollectionSpecies class >> encode: anOrderedCollection
with: anEncoder
onto: aStream [

	^anEncoder
		encodeOrderedCollection: anOrderedCollection
		on: aStream
]
