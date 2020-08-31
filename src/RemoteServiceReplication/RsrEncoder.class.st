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

	| error |
	error := aDeliverErrorResponse remoteError.
	^ByteArray
		streamContents:
			[:stream |
			self
				encodeControlWord: self deliverErrorResponseCommand
				onto: stream.
			self
				encodeControlWord: aDeliverErrorResponse transaction
				onto: stream.
			self
				encodeReferenceOf: error originalClassName
				onto: stream.
			self
				encodeReferenceOf: error tag
				onto: stream.
			self
				encodeReferenceOf: error messageText
				onto: stream.
			self
				encodeReferenceOf: error stack
				onto: stream]
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeDeliverResponse: aDeliverResponse [

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeControlWord: self deliverResponseCommand
				onto: stream.
			self
				encodeControlWord: aDeliverResponse transaction
				onto: stream.
			self
				encodeControlWord: aDeliverResponse services size
				onto: stream.
			aDeliverResponse services do: [:each | self encodeService: each onto: stream].
			self
				encodeReferenceOf: aDeliverResponse response
				onto: stream]
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeReferenceOf: anObject
onto: aStream [

	| species |
	species := self speciesOf: anObject.
	species
		encodeReference: anObject
		using: self
		onto: aStream
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeReleaseObjects: aReleaseObject [

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeControlWord: self releaseObjectsCommand
				onto: stream.
			self
				encodeControlWord: aReleaseObject oids size
				onto: stream.
			aReleaseObject oids
				do:
					[:oid |
					self
						encodeControlWord: oid
						onto: stream]]
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeSendMessage: aSendMessage [

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeControlWord: self sendMessageIdentifier
				onto: stream.
			self
				encodeControlWord: aSendMessage transaction
				onto: stream.
			self
				encodeControlWord: aSendMessage services size
				onto: stream.
			aSendMessage services
				do:
					[:each |
					self
						encodeServiceSnapshot: (RsrServiceSnapshot from: each)
						onto: stream].
			self
				encodeReferenceOf: aSendMessage receiver
				onto: stream.
			self
				encodeReferenceOf: aSendMessage selector
				onto: stream.
			self
				encodeControlWord: aSendMessage arguments size
				onto: stream.
			aSendMessage arguments
				do:
					[:each |
					self
						encodeReferenceOf: each
						onto: stream]]
]

{ #category : 'encoding' }
RsrEncoder >> encodeService: anObject [

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeService: anObject
				onto: stream]
]

{ #category : 'private-encoding' }
RsrEncoder >> encodeService: aService
onto: aStream [

	(aService isKindOf: RsrService)
		ifFalse: [^RsrUnsupportedObject signal: aService].
	self
		encodeServiceSnapshot: (RsrServiceSnapshot from: aService)
		onto: aStream
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

{ #category : 'private-testing' }
RsrEncoder >> isImmediate: anObject [

	^self speciesMapping includesKey: anObject class
]

{ #category : 'private-accessing' }
RsrEncoder >> sendMessageIdentifier [

	^1
]

{ #category : 'private-accessing' }
RsrEncoder >> speciesMapping [
	"Return a mapping between the native class and their associated RsrSpecies"

	^RsrSpecies speciesMapping
]

{ #category : 'accessing' }
RsrEncoder >> speciesOf: anObject [

	
	^RsrSpecies speciesOf: anObject
]
