Class {
	#name : 'RsrCodec',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication'
}

{ #category : 'private-accessing' }
RsrCodec >> controlWordMax [

	^(2 raisedTo: 63) - 1
]

{ #category : 'private-accessing' }
RsrCodec >> controlWordMin [

	^(2 raisedTo: 63) negated
]

{ #category : 'private-accessing-commands' }
RsrCodec >> deliverErrorResponseCommand [

	^4
]

{ #category : 'private-accessing-commands' }
RsrCodec >> deliverResponseCommand [

	^2
]

{ #category : 'private-accessing' }
RsrCodec >> immediateOID [

	^0
]

{ #category : 'private-accessing-commands' }
RsrCodec >> releaseObjectsCommand [

	^3
]

{ #category : 'private-accessing-commands' }
RsrCodec >> retainObjectCommand [

	^0
]

{ #category : 'private-accessing-commands' }
RsrCodec >> sendMessageCommand [

	^1
]

{ #category : 'private-accessing' }
RsrCodec >> sizeOfInteger [
	"Return the number of bytes used to encode an integer"

	^8
]
