Class {
	#name : #RsrUndefinedObjectSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrUndefinedObjectSpecies class >> encode: nilArgument
with: anEncoder
onto: aStream [

	^anEncoder encodeNilOnto: aStream
]
