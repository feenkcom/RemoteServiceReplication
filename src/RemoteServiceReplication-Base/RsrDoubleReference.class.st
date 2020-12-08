Class {
	#name : 'RsrDoubleReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'accessing' }
RsrDoubleReference class >> typeIdentifier [

	^15
]

{ #category : 'encoding/decoding' }
RsrDoubleReference >> decode: aStream
using: aDecoder [

	| bytes |
	bytes := aStream next: 8.
	value := self convertBytes: bytes
]

{ #category : 'encoding/decoding' }
RsrDoubleReference >> encode: aStream
using: anEncoder [

	| bytes |
	bytes := self convertToBytes: value.
	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self typeIdentifier
		onto: aStream.
	aStream nextPutAll: bytes
]
