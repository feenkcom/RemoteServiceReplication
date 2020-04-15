Class {
	#name : 'RsrNullSpecies',
	#superclass : 'RsrSpecies',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'object analysis' }
RsrNullSpecies class >> analyze: anObject
using: anAnalyzer [
	"A method that works in conjunction with RsrRetainAnalysis to analyze
	an object"

	^RsrUnsupportedObject signal: 'Unsupported object (' , anObject printString , ')'
]

{ #category : 'coding' }
RsrNullSpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	self shouldNotImplement
]

{ #category : 'coding' }
RsrNullSpecies class >> encode: anObject
using: anEncoder
on: aStream [
	"Encode this object. This is specifically used by RsrServiceSpecies."

	RsrUnsupportedObject signal: anObject
]

{ #category : 'coding' }
RsrNullSpecies class >> encodeReference: anObject
using: anEncoder
onto: aStream [

	^RsrUnsupportedObject signal: anObject
]

{ #category : 'accessing' }
RsrNullSpecies class >> speciesIdentifier [

	^self shouldNotImplement
]
