Class {
	#name : 'RsrCharacterReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'encoding/decoding' }
RsrCharacterReference >> decode: aStream
using: aDecoder [

	| codePoint |
	codePoint := aDecoder decodeControlWord: aStream.
	value := Character codePoint: codePoint
]

{ #category : 'encoding/decoding' }
RsrCharacterReference >> encode: aStream
using: anEncoder [

	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self typeIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: value codePoint
		onto: aStream
]

{ #category : 'accessing' }
RsrCharacterReference >> typeIdentifier [

	^5
]
