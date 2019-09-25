Class {
	#name : #RsrBooleanSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrBooleanSpecies class >> encodeReference: aBoolean
using: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream
]

{ #category : #accessing }
RsrBooleanSpecies class >> speciesIdentifier [

	^self subclassResponsibility
]
