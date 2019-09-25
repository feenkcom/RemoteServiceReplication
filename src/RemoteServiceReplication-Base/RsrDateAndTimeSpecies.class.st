Class {
	#name : #RsrDateAndTimeSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrDateAndTimeSpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	| microseconds |
	microseconds := aDecoder decodeControlWord: aStream.
	^self fromMicroseconds: microseconds
]

{ #category : #coding }
RsrDateAndTimeSpecies class >> encodeReference: aDateAndTime
using: anEncoder
onto: aStream [

	| microseconds |
	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	microseconds := self microsecondsSinceEpoch: aDateAndTime.
	anEncoder
		encodeControlWord: microseconds
		onto: aStream
]

{ #category : #accessing }
RsrDateAndTimeSpecies class >> speciesIdentifier [

	^14
]
