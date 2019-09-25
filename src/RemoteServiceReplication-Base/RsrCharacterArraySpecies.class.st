Class {
	#name : #RsrCharacterArraySpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrCharacterArraySpecies class >> decodeReference: aStream
for: aDecoder [
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
RsrCharacterArraySpecies class >> encodeReference: aCharacterArray
with: anEncoder
onto: aStream [

	| bytes |
	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	bytes := RsrEnvironment
		ifPharo: [aCharacterArray utf8Encoded]
		ifGemStone: [aCharacterArray encodeAsUTF8]
		ifDolphin: [aCharacterArray asUtf8String asByteArray].
	anEncoder
		encodeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes
]

{ #category : #accessing }
RsrCharacterArraySpecies class >> speciesIdentifier [

	^self subclassResponsibility
]
