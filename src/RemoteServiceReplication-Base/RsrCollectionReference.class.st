Class {
	#name : 'RsrCollectionReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'instance creation' }
RsrCollectionReference class >> from: aSequencedCollection [

	| references |
	references := (1 to: aSequencedCollection size) collect: [:i | RsrReference from: (aSequencedCollection at: i)].
	^self value: references
]

{ #category : 'encoding/decoding' }
RsrCollectionReference >> decode: aStream
using: aDecoder [

	| size |
	size := aDecoder decodeControlWord: aStream.
	value := (1 to: size) collect: [:i | aDecoder decodeReference: aStream]
]

{ #category : 'encoding/decoding' }
RsrCollectionReference >> encode: aStream
using: anEncoder [

	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self typeIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: value size
		onto: aStream.
	value
		do:
			[:each |
			each
				encode: aStream
				using: anEncoder]
]
