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
	#category : 'RemoteServiceReplication'
}

{ #category : 'converting' }
RsrCharacterArrayReference >> convertBytes: aByteArray [

	^aByteArray decodeFromUTF8ToString
]

{ #category : 'decoding' }
RsrCharacterArrayReference >> decode: aStream
using: aDecoder [

	| length bytes |
	length := aDecoder decodeControlWord: aStream.
	bytes := aStream next: length.
	value := self convertBytes: bytes
]
