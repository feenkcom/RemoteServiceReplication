Class {
	#name : 'RsrArraySpecies',
	#superclass : 'RsrSpecies',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'object analysis' }
RsrArraySpecies class >> analyze: anArray
using: anAnalyzer [
	"A method that works in conjunction with RsrRetainAnalysis to analyze
	an object"

	^anAnalyzer analyzeCollection: anArray
]

{ #category : 'coding' }
RsrArraySpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	| size array |
	size := aDecoder decodeControlWord: aStream.
	array := Array new: size.
	1 to: size do: [:i | array at: i put: (aDecoder decodeAndResolveObjectReference: aStream)].
	^array
]

{ #category : 'coding' }
RsrArraySpecies class >> encodeReference: anArray
using: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: anArray size
		onto: aStream.
	anArray
		do:
			[:each |
			anEncoder
				encodeReferenceOf: each
				onto: aStream]
]

{ #category : 'accessing' }
RsrArraySpecies class >> speciesIdentifier [

	^9
]
