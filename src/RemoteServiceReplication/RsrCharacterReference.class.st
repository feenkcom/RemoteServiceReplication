Class {
	#name : 'RsrCharacterReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'decoding' }
RsrCharacterReference >> decode: aStream
using: aDecoder [

	| codePoint |
	codePoint := aDecoder decodeControlWord: aStream.
	value := Character codePoint: codePoint
]
