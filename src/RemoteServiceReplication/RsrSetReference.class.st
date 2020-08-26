Class {
	#name : 'RsrSetReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'decoding' }
RsrSetReference >> decode: aStream
using: aDecoder [

	| size |
	size := aDecoder decodeControlWord: aStream.
	value :=  (1 to: size) collect: [:i | aDecoder decodeReference: aStream]
]

{ #category : 'resolving' }
RsrSetReference >> resolve: aRegistry [

	| set |
	set := Set new.
	value do: [:each | set add: (each resolve: aRegistry)].
	^set
]
