Class {
	#name : 'RsrEncoder',
	#superclass : 'RsrCodec',
	#category : 'RemoteServiceReplication'
}

{ #category : 'private-encoding' }
RsrEncoder >> encodeControlWord: anInteger
onto: aStream [

	| encodedInteger encodedBytes |
	(anInteger between: self controlWordMin and: self controlWordMax)
		ifFalse: [self error: anInteger printString, ' is outside the supported size of a control word.'].
	encodedInteger := (anInteger positive
		ifTrue: [anInteger]
		ifFalse: [(2 raisedTo: 64) + anInteger]).
	encodedBytes := self
		integerAsByteArray: encodedInteger
		ofSize: self sizeOfInteger.
	aStream nextPutAll: encodedBytes
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeDeliverErrorResponse: aDeliverErrorResponse [

	^ByteArray streamContents: [:stream | self encodeDeliverErrorResponse: aDeliverErrorResponse onto: stream]
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeDeliverErrorResponse: aDeliverErrorResponse
onto: aStream [

	| error |
	error := aDeliverErrorResponse remoteError.
	self
		encodeControlWord: self deliverErrorResponseCommand
		onto: aStream.
	self
		encodeControlWord: aDeliverErrorResponse transaction
		onto: aStream.
	self
		encodeReference: (RsrReference from: error originalClassName)
		onto: aStream.
	self
		encodeReference: (RsrReference from: error tag)
		onto: aStream.
	self
		encodeReference: (RsrReference from: error messageText)
		onto: aStream.
	self
		encodeReference: (RsrReference from: error stack)
		onto: aStream
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeDeliverResponse: aDeliverResponse [

	^ByteArray streamContents: [:stream | self encodeDeliverResponse: aDeliverResponse onto: stream]
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeDeliverResponse: aDeliverResponse
onto: aStream [

	self
		encodeControlWord: self deliverResponseCommand
		onto: aStream.
	self
		encodeControlWord: aDeliverResponse transaction
		onto: aStream.
	self
		encodeControlWord: aDeliverResponse snapshots size
		onto: aStream.
	aDeliverResponse snapshots do: [:each | self encodeServiceSnapshot: each onto: aStream].
	self
		encodeReference: aDeliverResponse response
		onto: aStream
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeReference: aReference
onto: aStream [

	aReference
		encode: aStream
		using: self
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeReleaseServices: aReleaseServices [

	^ByteArray streamContents: [:stream | self encodeReleaseServices: aReleaseServices onto: stream]
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeReleaseServices: aReleaseServices
onto: aStream [

	self
		encodeControlWord: self releaseObjectsCommand
		onto: aStream.
	self
		encodeControlWord: aReleaseServices sids size
		onto: aStream.
	aReleaseServices sids
		do:
			[:sid |
			self
				encodeControlWord: sid
				onto: aStream]
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeSendMessage: aSendMessage [

	^ByteArray streamContents: [:stream | self encodeSendMessage: aSendMessage onto: stream]
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeSendMessage: aSendMessage
onto: aStream [

	self
		encodeControlWord: self sendMessageCommand
		onto: aStream.
	self
		encodeControlWord: aSendMessage transaction
		onto: aStream.
	self
		encodeControlWord: aSendMessage snapshots size
		onto: aStream.
	aSendMessage snapshots
		do:
			[:each |
			self
				encodeServiceSnapshot: each
				onto: aStream].
	self
		encodeReference:  aSendMessage receiverReference
		onto: aStream.
	self
		encodeReference: aSendMessage selectorReference
		onto: aStream.
	self
		encodeControlWord: aSendMessage argumentReferences size
		onto: aStream.
	aSendMessage argumentReferences
		do:
			[:each |
			self
				encodeReference: each
				onto: aStream]
]

{ #category : 'encoding' }
RsrEncoder >> encodeServiceSnapshot: aServiceSnapshot [

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeServiceSnapshot: aServiceSnapshot
				onto: stream]
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeServiceSnapshot: aServiceSnapshot
onto: aStream [

	aServiceSnapshot
		encode: aStream
		using: self
]

{ #category : 'converting' }
RsrEncoder >> integerAsByteArray: anInteger
ofSize: aNumberOfBytes [

	| bytes int |
	bytes := ByteArray new: aNumberOfBytes.
	int := anInteger.
	aNumberOfBytes
		to: 1
		by: -1
		do:
			[:i | | byte |
			byte := int bitAnd: 16rFF.
			int := int bitShift: -8.
			bytes at: i put: byte].
	int ~= 0
		ifTrue: [self error: 'Loss of precision detected'].
	^bytes
]
