Class {
	#name : #RsrArraySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrArraySpecies class >> encode: anArray
with: anEncoder
onto: aStream [

	^anEncoder
		encodeArray: anArray
		on: aStream
]
