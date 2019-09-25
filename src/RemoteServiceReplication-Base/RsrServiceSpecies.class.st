Class {
	#name : #RsrServiceSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrServiceSpecies class >> encodeReference: aService
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: aService _id
		onto: aStream
]

{ #category : #accessing }
RsrServiceSpecies class >> speciesIdentifier [

	^0
]
