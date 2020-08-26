Class {
	#name : 'RsrNilReference',
	#superclass : 'RsrImmediateReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'decoding' }
RsrNilReference >> decode: aStream
using: aDecoder [

	"Nil has no additional value"
]

{ #category : 'resolving' }
RsrNilReference >> resolve: aRegistry [

	^nil
]
