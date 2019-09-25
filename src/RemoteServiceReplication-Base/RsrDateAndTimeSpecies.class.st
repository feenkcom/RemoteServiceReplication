Class {
	#name : #RsrDateAndTimeSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrDateAndTimeSpecies class >> decodeWith: aDecoder
from: aStream [
	"Decode the provided bytes into the default native class for this species"

	| microseconds |
	microseconds := aDecoder decodeControlWord: aStream.
	^RsrDateAndTimeInterface fromMicroseconds: microseconds
]

{ #category : #coding }
RsrDateAndTimeSpecies class >> encodeReference: aDateAndTime
with: anEncoder
onto: aStream [

	| microseconds |
	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	microseconds := RsrDateAndTimeInterface microsecondsSince: aDateAndTime.
	anEncoder
		encodeControlWord: microseconds
		onto: aStream
]

{ #category : #accessing }
RsrDateAndTimeSpecies class >> speciesIdentifier [

	^14
]
