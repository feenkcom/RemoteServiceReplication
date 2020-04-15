Class {
	#name : 'RsrByteArraySpecies',
	#superclass : 'RsrSpecies',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'coding' }
RsrByteArraySpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	| size |
	size := aDecoder decodeControlWord: aStream.
	^aStream next: size
]

{ #category : 'coding' }
RsrByteArraySpecies class >> encodeReference: aByteArray
using: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: aByteArray size
		onto: aStream.
	aStream nextPutAll: aByteArray
]

{ #category : 'accessing' }
RsrByteArraySpecies class >> speciesIdentifier [

	^10
]
