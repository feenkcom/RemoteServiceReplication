Class {
	#name : #RsrIntegerSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrIntegerSpecies class >> encodeReference: anInteger
with: anEncoder
onto: aStream [

	| bytes |
	bytes := anEncoder integerAsByteArray: anInteger abs.
	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes
]

{ #category : #accessing }
RsrIntegerSpecies class >> speciesIdentifier [

	^self subclassResponsibility
]
