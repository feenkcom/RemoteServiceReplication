Class {
	#name : #RsrDictionarySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #'object analysis' }
RsrDictionarySpecies class >> analyze: aDictionary
using: anAnalyzer [
	"A method that works in conjunction with RsrRetainAnalysis to analyze
	an object"

	^anAnalyzer analyzeDictionary: aDictionary
]

{ #category : #coding }
RsrDictionarySpecies class >> decodeReference: aStream
using: aDecoder [
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
using: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: anEncoder immediateOID
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
