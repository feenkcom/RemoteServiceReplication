Class {
	#name : #RsrOrderedCollectionSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #'object analysis' }
RsrOrderedCollectionSpecies class >> analyze: anOrderedCollection
using: anAnalyzer [
	"A method that works in conjunction with RsrRetainAnalysis to analyze
	an object"

	^anAnalyzer analyzeCollection: anOrderedCollection
]

{ #category : #coding }
RsrOrderedCollectionSpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	| size oc |
	size := aDecoder decodeControlWord: aStream.
	oc := OrderedCollection new: size.
	size timesRepeat: [oc add: (aDecoder decodeObjectReference: aStream)].
	^oc
]

{ #category : #coding }
RsrOrderedCollectionSpecies class >> encodeReference: anOrderedCollection
using: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: anOrderedCollection size
		onto: aStream.
	anOrderedCollection
		do:
			[:each |
			anEncoder
				encodeReferenceOf: each
				onto: aStream]
]

{ #category : #accessing }
RsrOrderedCollectionSpecies class >> speciesIdentifier [

	^12
]
