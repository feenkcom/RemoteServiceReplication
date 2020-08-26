Class {
	#name : 'RsrOrderedCollectionReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'decoding' }
RsrOrderedCollectionReference >> decode: aStream
using: aDecoder [

	| size |
	size := aDecoder decodeControlWord: aStream.
	value := OrderedCollection new: size.
	size timesRepeat: [value add: (aDecoder decodeReference: aStream)]
]

{ #category : 'resolving' }
RsrOrderedCollectionReference >> resolve: aRegistry [

	^value collect: [:each | each resolve: aRegistry]
]
