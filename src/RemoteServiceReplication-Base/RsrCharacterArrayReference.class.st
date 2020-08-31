"
No class-specific documentation for RsrCharacterArrayReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrCharacterArrayReference

"
Class {
	#name : 'RsrCharacterArrayReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'converting' }
RsrCharacterArrayReference >> convertBytes: aByteArray [

	^aByteArray decodeFromUTF8ToString
]

{ #category : 'converting' }
RsrCharacterArrayReference >> convertToBytes: aCharacterArray [

	^aCharacterArray encodeAsUTF8
]

{ #category : 'encoding/decoding' }
RsrCharacterArrayReference >> decode: aStream
using: aDecoder [

	| length bytes |
	length := aDecoder decodeControlWord: aStream.
	bytes := aStream next: length.
	value := self convertBytes: bytes
]

{ #category : 'encoding/decoding' }
RsrCharacterArrayReference >> encode: aStream
using: anEncoder [

	| bytes |
	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self typeIdentifier
		onto: aStream.
	bytes := self convertToBytes: value.
	anEncoder
		encodeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes
]
