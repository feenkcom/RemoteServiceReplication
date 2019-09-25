Class {
	#name : #RsrByteArraySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrByteArraySpecies class >> encodeReference: aByteArray
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: aByteArray size
		onto: aStream.
	aStream nextPutAll: aByteArray
]

{ #category : #accessing }
RsrByteArraySpecies class >> speciesIdentifier [

	^10
]
