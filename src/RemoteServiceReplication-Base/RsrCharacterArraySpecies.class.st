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

{ #category : #accessing }
RsrCharacterArraySpecies class >> speciesIdentifier [

	^self subclassResponsibility
]
