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
	#category : 'RemoteServiceReplication'
}

{ #category : 'converting' }
RsrIntegerReference >> convertBytes: aByteArray [

	^aByteArray
		inject: 0
		into: [:integer :byte | (integer bitShift: 8) bitOr: byte]
]

{ #category : 'decoding' }
RsrIntegerReference >> decode: aStream
using: aDecoder [

	| length bytes |
	length := aDecoder decodeControlWord: aStream.
	bytes := aStream next: length.
	value := self convertBytes: bytes
]
