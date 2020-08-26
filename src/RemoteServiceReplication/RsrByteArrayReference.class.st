Class {
	#name : 'RsrByteArrayReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'decoding' }
RsrByteArrayReference >> decode: aStream
using: aDecoder [

	| length |
	length := aDecoder decodeControlWord: aStream.
	value := aStream next: length
]
