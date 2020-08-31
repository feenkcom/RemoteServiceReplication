Class {
	#name : 'RsrDateAndTimeReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'accessing' }
RsrDateAndTimeReference class >> typeIdentifier [

	^14
]

{ #category : 'calculating' }
RsrDateAndTimeReference >> calculateFromMicroSeconds: anInteger [

    ^DateAndTime
        posixSeconds: (anInteger / 1000000)
        offset: Duration zero
]

{ #category : 'encoding/decoding' }
RsrDateAndTimeReference >> decode: aStream
using: aDecoder [

	| microseconds |
	microseconds := aDecoder decodeControlWord: aStream.
	value := self calculateFromMicroSeconds: microseconds
]

{ #category : 'encoding/decoding' }
RsrDateAndTimeReference >> encode: aStream
using: anEncoder [

	| microseconds |
	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self typeIdentifier
		onto: aStream.
	microseconds := self microsecondsSinceEpoch: value.
	anEncoder
		encodeControlWord: microseconds
		onto: aStream
]

{ #category : 'calculating' }
RsrDateAndTimeReference >> microsecondsSinceEpoch: aDateAndTime [

	^((aDateAndTime asSeconds - self posixEpoch asSeconds) * 1000000) rounded
]

{ #category : 'calculating' }
RsrDateAndTimeReference >> posixEpoch [

	^DateAndTime
        posixSeconds: 0
        offset: Duration zero
]
