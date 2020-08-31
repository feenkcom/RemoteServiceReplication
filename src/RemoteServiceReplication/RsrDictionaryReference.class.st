"
No class-specific documentation for RsrDictionaryReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrValueReference( value)
          RsrDictionaryReference

"
Class {
	#name : 'RsrDictionaryReference',
	#superclass : 'RsrCollectionReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrDictionaryReference class >> from: aDictionary [

	| referenceStream |
	referenceStream := WriteStream on: (Array new: aDictionary size * 2).
	aDictionary
		keysAndValuesDo:
			[:key :value |
			referenceStream
				nextPut: (RsrReference from: key);
				nextPut: (RsrReference from: value)].
	^self value: referenceStream contents
]

{ #category : 'accessing' }
RsrDictionaryReference class >> typeIdentifier [

	^13
]

{ #category : 'encoding/decoding' }
RsrDictionaryReference >> decode: aStream
using: aDecoder [

	| size |
	size := aDecoder decodeControlWord: aStream.
	value := (1 to: size * 2) collect: [:each | aDecoder decodeReference: aStream]
]

{ #category : 'encoding/decoding' }
RsrDictionaryReference >> encode: aStream
using: anEncoder [

	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self typeIdentifier
		onto: aStream.
	anEncoder
		encodeControlWord: value size / 2
		onto: aStream.
	value do: [:each | each encode: aStream using: anEncoder]
]

{ #category : 'resolving' }
RsrDictionaryReference >> resolve: aRegistry [

	| stream numEntries dictionary |
	stream := ReadStream on: value.
	numEntries := value size / 2.
	dictionary := Dictionary new: numEntries.
	numEntries
		timesRepeat:
			[dictionary
				at: (stream next resolve: aRegistry)
				put: (stream next resolve: aRegistry)].
	^dictionary
]
