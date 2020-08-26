"
No class-specific documentation for RsrTrueReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference( value)
        RsrTrueReference

"
Class {
	#name : 'RsrTrueReference',
	#superclass : 'RsrImmediateReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'decoding' }
RsrTrueReference >> decode: aStream
using: aDecoder [

	"True has no additional value"
]

{ #category : 'resolving' }
RsrTrueReference >> resolve: aRegistry [

	^true
]
