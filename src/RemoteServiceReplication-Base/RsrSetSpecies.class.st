Class {
	#name : #RsrSetSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrSetSpecies class >> encodeReference: aSet
with: anEncoder
onto: aStream [

	^anEncoder
		encodeSet: aSet
		on: aStream
]
