Class {
	#name : #RsrArraySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrArraySpecies class >> encodeReference: anArray
with: anEncoder
onto: aStream [

	^anEncoder
		encodeArray: anArray
		onto: aStream
]

{ #category : #accessing }
RsrArraySpecies class >> speciesIdentifier [

	^9
]
