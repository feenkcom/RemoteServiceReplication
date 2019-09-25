Class {
	#name : #RsrUndefinedObjectSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrUndefinedObjectSpecies class >> encodeReference: nilArgument
with: anEncoder
onto: aStream [

	^anEncoder encodeNilOnto: aStream
]

{ #category : #accessing }
RsrUndefinedObjectSpecies class >> speciesIdentifier [

	^6
]
