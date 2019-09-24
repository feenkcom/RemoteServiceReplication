Class {
	#name : #RsrServiceSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrServiceSpecies class >> encodeReference: aService
with: anEncoder
onto: aStream [

	^anEncoder
		encodeServiceReference: aService
		onto: aStream
]
