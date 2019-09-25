Class {
	#name : #RsrBooleanSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrBooleanSpecies class >> encodeReference: aBoolean
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream
]

{ #category : #accessing }
RsrBooleanSpecies class >> speciesIdentifier [

	^self subclassResponsibility
]
