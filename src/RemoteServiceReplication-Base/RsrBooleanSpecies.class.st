Class {
	#name : #RsrBooleanSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrBooleanSpecies class >> encodeReference: aBoolean
with: anEncoder
onto: aStream [

	^aBoolean
		ifTrue: [anEncoder encodeTrueOnto: aStream]
		ifFalse: [anEncoder encodeFalseOnto: aStream]
]
