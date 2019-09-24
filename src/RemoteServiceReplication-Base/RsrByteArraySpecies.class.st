Class {
	#name : #RsrByteArraySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrByteArraySpecies class >> encode: aByteArray
with: anEncoder
onto: aStream [

	^anEncoder
		encodeByteArray: aByteArray
		on: aStream
]
