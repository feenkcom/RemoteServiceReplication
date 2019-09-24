Class {
	#name : #RsrByteArraySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrByteArraySpecies class >> encodeReference: aByteArray
with: anEncoder
onto: aStream [

	^anEncoder
		encodeByteArray: aByteArray
		on: aStream
]
