Class {
	#name : 'RsrNilReference',
	#superclass : 'RsrImmediateReference',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'instance creation' }
RsrNilReference class >> from: aNil [

	^self new
]

{ #category : 'encoding/decoding' }
RsrNilReference >> decode: aStream
using: aDecoder [

	"Nil has no additional value"
]

{ #category : 'encoding/decoding' }
RsrNilReference >> encode: aStream
using: anEncoder [

	anEncoder
		encodeControlWord: anEncoder immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self typeIdentifier
		onto: aStream
]

{ #category : 'resolving' }
RsrNilReference >> resolve: aRegistry [

	^nil
]

{ #category : 'accessing' }
RsrNilReference >> typeIdentifier [

	^6
]
