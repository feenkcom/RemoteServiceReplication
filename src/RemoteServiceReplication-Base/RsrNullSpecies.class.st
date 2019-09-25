Class {
	#name : #RsrNullSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrNullSpecies class >> decode: bytes [

	self shouldNotImplement
]

{ #category : #coding }
RsrNullSpecies class >> encodeReference: anObject
with: anEncoder
onto: aStream [

	^RsrUnsupportedObject signal: anObject
]

{ #category : #accessing }
RsrNullSpecies class >> speciesIdentifier [

	^self shouldNotImplement
]
