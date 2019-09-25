Class {
	#name : #RsrStringSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrStringSpecies class >> encodeReference: aString
with: anEncoder
onto: aStream [

	^anEncoder
		encodeString: aString
		onto: aStream
]

{ #category : #accessing }
RsrStringSpecies class >> speciesIdentifier [

	^2
]
