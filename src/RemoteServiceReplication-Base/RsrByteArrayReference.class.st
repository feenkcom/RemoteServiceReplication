"
No class-specific documentation for RsrByteArrayReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrValueReference( value)
          RsrByteArrayReference

"
Class {
	#name : 'RsrByteArrayReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'encoding/decoding' }
RsrByteArrayReference >> decode: aStream
using: aDecoder [

	| length |
	length := aDecoder decodeControlWord: aStream.
	value := aStream next: length
]

{ #category : 'encoding/decoding' }
RsrByteArrayReference >> encode: aStream
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
	aStream nextPutAll: value
]

{ #category : 'accessing' }
RsrByteArrayReference >> typeIdentifier [

	^10
]
