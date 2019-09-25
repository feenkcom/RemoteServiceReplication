Class {
	#name : #RsrStringSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrStringSpecies class >> decodeWith: aDecoder
from: aStream [
	"Decode the provided bytes into the default native class for this species"

	| length bytes |
	length := aDecoder decodeControlWord: aStream.
	bytes := aStream next: length.
	^RsrEnvironment
		ifPharo: [bytes utf8Decoded]
		ifGemStone: [bytes decodeFromUTF8ToString]
		ifDolphin: [(RsrClassResolver classNamed: #Utf8String) fromByteArray: bytes]
]

{ #category : #coding }
RsrStringSpecies class >> encodeReference: aString
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeStringBody: aString
		onto: aStream
]

{ #category : #accessing }
RsrStringSpecies class >> speciesIdentifier [

	^2
]
