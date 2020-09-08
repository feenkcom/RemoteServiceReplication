"
No class-specific documentation for RsrDateAndTimeReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrValueReference( value)
          RsrDateAndTimeReference

"
Class {
	#name : 'RsrDateAndTimeReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'accessing' }
RsrDateAndTimeReference class >> typeIdentifier [

	^14
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
