Class {
	#name : 'RsrArrayReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'decoding' }
RsrArrayReference >> decode: aStream
using: aDecoder [

	| size |
	size := aDecoder decodeControlWord: aStream.
	value := Array new: size.
	1 to: size do: [:i | value at: i put: (aDecoder decodeReference: aStream)]
]

{ #category : 'resolving' }
RsrArrayReference >> resolve: aRegistry [

	^value collect: [:each | each resolve: aRegistry]
]
