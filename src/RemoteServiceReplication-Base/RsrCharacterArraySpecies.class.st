Class {
	#name : 'RsrCharacterArraySpecies',
	#superclass : 'RsrSpecies',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'coding' }
RsrCharacterArraySpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	| length bytes |
	length := aDecoder decodeControlWord: aStream.
	bytes := aStream next: length.
	^self fromBytes: bytes
]

{ #category : 'coding' }
RsrCharacterArraySpecies class >> encodeReference: aCharacterArray
using: anEncoder
onto: aStream [

	| bytes |
	anEncoder
		encodeControlWord: anEncoder immediateOID
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

{ #category : 'accessing' }
RsrCharacterArraySpecies class >> speciesIdentifier [

	^self subclassResponsibility
]
