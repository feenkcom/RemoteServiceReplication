Class {
	#name : #RsrSetSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrSetSpecies class >> decodeReference: aStream
for: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	| size set |
	size := aDecoder decodeControlWord: aStream.
	set := Set new: size.
	size timesRepeat: [set add: (aDecoder decodeObjectReference: aStream)].
	^set
]

{ #category : #coding }
RsrSetSpecies class >> encodeReference: aSet
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: aSet size
		onto: aStream.
	aSet
		do:
			[:each |
			anEncoder
				encodeReferenceOf: each
				onto: aStream]
]

{ #category : #accessing }
RsrSetSpecies class >> speciesIdentifier [

	^11
]
