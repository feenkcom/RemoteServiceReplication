Class {
	#name : #RsrIntegerSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrIntegerSpecies class >> encodeReference: anInteger
with: anEncoder
onto: aStream [

	^anEncoder
		encodeImmediateInteger: anInteger
		onto: aStream
]
