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
	^self fromBytes: bytes
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
	bytes := self toBytes: aCharacterArray.
	anEncoder
		encodeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes
]

{ #category : #converting }
RsrCharacterArraySpecies class >> fromBytes: aByteArray [

	^RsrEnvironment
		ifPharo: [aByteArray utf8Decoded]
		ifGemStone: [aByteArray decodeFromUTF8ToString]
		ifDolphin: [(RsrClassResolver classNamed: #Utf8String) fromByteArray: aByteArray]
]

{ #category : #accessing }
RsrCharacterArraySpecies class >> speciesIdentifier [

	^self subclassResponsibility
]

{ #category : #converting }
RsrCharacterArraySpecies class >> toBytes: aCharacterArray [

	^RsrEnvironment
		ifPharo: [aCharacterArray utf8Encoded]
		ifGemStone: [aCharacterArray encodeAsUTF8]
		ifDolphin: [aCharacterArray asUtf8String asByteArray].
]
