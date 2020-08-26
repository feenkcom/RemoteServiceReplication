Class {
	#name : 'RsrFalseReference',
	#superclass : 'RsrImmediateReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'decoding' }
RsrFalseReference >> decode: aStream
using: aDecoder [

	"False has no additional value"
]

{ #category : 'resolving' }
RsrFalseReference >> resolve: aRegistry [

	^false
]
