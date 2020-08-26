Class {
	#name : 'RsrDictionaryReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'decoding' }
RsrDictionaryReference >> decode: aStream
using: aDecoder [

	| size |
	size := aDecoder decodeControlWord: aStream.
	value := (1 to: size * 2) collect: [:each | aDecoder decodeReference: aStream]
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
