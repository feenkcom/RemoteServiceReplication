Class {
	#name : #RsrNullSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrNullSpecies class >> decodeReference: aStream
for: aDecoder [
	"Decode the provided bytes into the default native class for this species"

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
