Class {
	#name : #RsrStringSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrStringSpecies class >> encode: aString
with: anEncoder
onto: aStream [

	^anEncoder
		encodeString: aString
		onto: aStream
]
