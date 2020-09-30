"
No class-specific documentation for RsrSetReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrValueReference( value)
          RsrSetReference

"
Class {
	#name : 'RsrSetReference',
	#superclass : 'RsrCollectionReference',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'instance creation' }
RsrSetReference class >> from: aSet [

	| referenceStream |
	referenceStream := WriteStream on: (Array new: aSet size).
	aSet do:  [:each | referenceStream nextPut: (RsrReference from: each)].
	^self value: referenceStream contents
]

{ #category : 'accessing' }
RsrSetReference class >> typeIdentifier [

	^11
]

{ #category : 'encoding/decoding' }
RsrSetReference >> decode: aStream
using: aDecoder [

	| size |
	size := aDecoder decodeControlWord: aStream.
	value :=  (1 to: size) collect: [:i | aDecoder decodeReference: aStream]
]

{ #category : 'encoding/decoding' }
RsrSetReference >> encode: aStream
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

{ #category : 'resolving' }
RsrSetReference >> resolve: aConnection [

	| set |
	set := Set new.
	value do: [:each | set add: (each resolve: aConnection)].
	^set
]
