"
No class-specific documentation for RsrIntegerReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrIntegerReference( value)

"
Class {
	#name : 'RsrIntegerReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'instance creation' }
RsrIntegerReference class >> from: anInteger [

	^anInteger positive
		ifTrue: [RsrPositiveIntegerReference value: anInteger]
		ifFalse: [RsrNegativeIntegerReference value: anInteger]
]

{ #category : 'converting' }
RsrIntegerReference >> convertBytes: aByteArray [

	^aByteArray
		inject: 0
		into: [:integer :byte | (integer bitShift: 8) bitOr: byte]
]

{ #category : 'converting' }
RsrIntegerReference >> convertToBytes: anInteger [

	| stream int |
	anInteger <= 0
		ifTrue: [^#[0]].
	stream := WriteStream on: (ByteArray new: 8).
	int := anInteger.
	[int > 0]
		whileTrue:
			[stream nextPut: (int bitAnd: 16rFF).
			int := int bitShift: -8].
	^stream contents reverse
]

{ #category : 'encoding/decoding' }
RsrIntegerReference >> decode: aStream
using: aDecoder [

	| length bytes |
	length := aDecoder decodeControlWord: aStream.
	bytes := aStream next: length.
	value := self convertBytes: bytes
]

{ #category : 'encoding/decoding' }
RsrIntegerReference >> encode: aStream
using: anEncoder [

	| bytes |
	bytes := self convertToBytes: value abs.
	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self typeIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes
]
