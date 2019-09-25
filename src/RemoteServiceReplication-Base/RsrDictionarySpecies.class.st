Class {
	#name : #RsrDictionarySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrDictionarySpecies class >> decodeWith: aDecoder
from: aStream [
	"Decode the provided bytes into the default native class for this species"

	| size dictionary |
	size := aDecoder decodeControlWord: aStream.
	dictionary := Dictionary new: size.
	1
		to: size
		do:
			[:i |
			dictionary
				at: (aDecoder decodeObjectReference: aStream)
				put: (aDecoder decodeObjectReference: aStream)].
	^dictionary
]

{ #category : #coding }
RsrDictionarySpecies class >> encodeReference: aDictionary
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: aDictionary size
		onto: aStream.
	aDictionary
		keysAndValuesDo:
			[:key :value |
			anEncoder
				encodeReferenceOf: key
				onto: aStream.
			anEncoder
				encodeReferenceOf: value
				onto: aStream]
]

{ #category : #accessing }
RsrDictionarySpecies class >> speciesIdentifier [

	^13
]
