Class {
	#name : #RsrUndefinedObjectSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrUndefinedObjectSpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	^nil
]

{ #category : #coding }
RsrUndefinedObjectSpecies class >> encodeReference: nilArgument
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
RsrUndefinedObjectSpecies class >> speciesIdentifier [

	^6
]
