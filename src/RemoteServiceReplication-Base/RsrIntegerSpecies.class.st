Class {
	#name : #RsrIntegerSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrIntegerSpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	| length bytes |
	length := aDecoder decodeControlWord: aStream.
	bytes := aStream next: length.
	^aDecoder bytesAsInteger: bytes
]

{ #category : #coding }
RsrIntegerSpecies class >> encodeReference: anInteger
using: anEncoder
onto: aStream [

	| bytes |
	bytes := self integerAsByteArray: anInteger abs.
	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes
]

{ #category : #converting }
RsrIntegerSpecies class >> integerAsByteArray: anInteger [
	"Return a ByteArray representing <anInteger> in big endian format."

	| stream int |
	anInteger <= 0
		ifTrue: [^ByteArray with: 0].
	stream := WriteStream on: (ByteArray new: 8).
	int := anInteger.
	[int > 0]
		whileTrue:
			[stream nextPut: (int bitAnd: 16rFF).
			int := int bitShift: -8].
	^stream contents reverse
]

{ #category : #accessing }
RsrIntegerSpecies class >> speciesIdentifier [

	^self subclassResponsibility
]
