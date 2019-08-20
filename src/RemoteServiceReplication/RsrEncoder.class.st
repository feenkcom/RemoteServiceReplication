Class {
	#name : #RsrEncoder,
	#superclass : #RsrCodec,
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'private-encoding' }
RsrEncoder >> encodeArray: anArray
on: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self arrayType
		onto: aStream.
	self
		encodeControlWord: anArray size
		onto: aStream.
	anArray
		do:
			[:each |
			self
				encodeReferenceOf: each
				onto: aStream]
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeByteArray: aByteArray
on: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self byteArrayType
		onto: aStream.
	self
		encodeControlWord: aByteArray size
		onto: aStream.
	aStream nextPutAll: aByteArray
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeCharacter: aCharacter
onto: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self characterType
		onto: aStream.
	self
		encodeControlWord: aCharacter codePoint
		onto: aStream
]

{ #category : #'private-encoding' }
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

{ #category : #'private-encoding' }
RsrEncoder >> encodeDateTime: aDateTime
on: aStream [

	| seconds microseconds |
	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self dateTimeType
		onto: aStream.
	seconds := aDateTime asSeconds - DateAndTime unixEpoch asSeconds.
	microseconds := seconds * 1000000 + (aDateTime nanoSecond / 1000) rounded.
	self
		encodeControlWord: microseconds
		onto: aStream
]

{ #category : #'private-encoding' }
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
				encodeReferenceOf: aDeliverResponse errorName
				onto: stream.
			self
				encodeReferenceOf: aDeliverResponse response
				onto: stream]
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeDictionary: aDictionary
on: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self dictionaryType
		onto: aStream.
	self
		encodeControlWord: aDictionary size
		onto: aStream.
	aDictionary
		keysAndValuesDo:
			[:key :value |
			self
				encodeReferenceOf: key
				onto: aStream.
			self
				encodeReferenceOf: value
				onto: aStream]
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeFalseOnto: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self falseType
		onto: aStream
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeImmediate: anImmediate
onto: aStream [

	anImmediate isSymbol
		ifTrue:
			[^self
				encodeSymbol: anImmediate
				onto: aStream].
	anImmediate isString
		ifTrue:
			[^self
				encodeString: anImmediate
				onto: aStream].
	anImmediate isInteger
		ifTrue:
			[^self
				encodeImmediateInteger: anImmediate
				onto: aStream].
	anImmediate isCharacter
		ifTrue:
			[^self
				encodeCharacter: anImmediate
				onto: aStream].
	anImmediate == nil
		ifTrue:
			[^self encodeNilOnto: aStream].
	anImmediate == true
		ifTrue:
			[^self encodeTrueOnto: aStream].
	anImmediate == false
		ifTrue:
			[^self encodeFalseOnto: aStream].
	anImmediate isArray
		ifTrue: [^self encodeArray: anImmediate on: aStream].
	anImmediate isDictionary
		ifTrue:
			[^self
				encodeDictionary: anImmediate
				on: aStream].
	anImmediate class == ByteArray
		ifTrue:
			[^self
				encodeByteArray: anImmediate
				on: aStream].
	anImmediate class == Set
		ifTrue:
			[^self
				encodeSet: anImmediate
				on: aStream].
	anImmediate class == OrderedCollection
		ifTrue:
			[^self
				encodeOrderedCollection: anImmediate
				on: aStream].
	anImmediate class == DateAndTime
		ifTrue:
			[^self
				encodeDateTime: anImmediate
				on: aStream].
	self error: 'Unsupported Immediate'
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeImmediateInteger: anInteger
onto: aStream [

	| bytes immediateType |
	immediateType := anInteger positive
		ifTrue: [self positiveIntegerType]
		ifFalse: [self negativeIntegerType].
	bytes := self integerAsByteArray: anInteger abs.
	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: immediateType
		onto: aStream.
	self
		encodeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeNilOnto: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self nilType
		onto: aStream
]

{ #category : #encoding }
RsrEncoder >> encodeObject: anObject [

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeObject: anObject
				onto: stream]
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeObject: anObject
onto: aStream [

	(self isService: anObject)
		ifTrue: [^self encodeService: anObject on: aStream].
	self error: 'Unable to encode: ', anObject printString
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeOrderedCollection: anOrderedCollection
on: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self orderedCollectionType
		onto: aStream.
	self
		encodeControlWord: anOrderedCollection size
		onto: aStream.
	anOrderedCollection
		do:
			[:each |
			self
				encodeReferenceOf: each
				onto: aStream]
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeReferenceOf: anObject
onto: aStream [

	(self isService: anObject)
		ifTrue: [^self encodeControlWord: anObject rsrId onto: aStream].
	(self isImmediate: anObject)
		ifTrue: [^self encodeImmediate: anObject onto: aStream].
	self error: 'Unsupported type'
]

{ #category : #'private-encoding' }
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

{ #category : #'private-encoding' }
RsrEncoder >> encodeRetainObject: aRetainObject [

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeControlWord: self retainObjectIdentifier
				onto: stream.
			self
				encodeObject: aRetainObject object
				onto: stream]
]

{ #category : #'private-encoding' }
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
				encodeControlWord: aSendMessage arguments size
				onto: stream.
			self
				encodeReferenceOf: aSendMessage receiver
				onto: stream.
			self
				encodeReferenceOf: aSendMessage selector
				onto: stream.
			aSendMessage arguments
				do:
					[:each |
					self
						encodeReferenceOf: each
						onto: stream]]
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeService: aService
on: aStream [

	"type"
	"the OID for the object"
	"the name of the remote service to create"
	"Write the object slots"
	| reflectedVariables |
	reflectedVariables := self reflectedVariableNamesOf: aService.
	self
		encodeControlWord: self serviceType
		onto: aStream.
	self
		encodeControlWord: aService rsrId
		onto: aStream.
	self
		encodeControlWord: reflectedVariables size
		onto: aStream.
	self
		encodeSymbol: aService remoteServiceName
		onto: aStream.
	self
		reflectedVariablesOf: aService
		do: [:each | self encodeReferenceOf: each onto: aStream]
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeSet: aSet
on: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self setType
		onto: aStream.
	self
		encodeControlWord: aSet size
		onto: aStream.
	aSet
		do:
			[:each |
			self
				encodeReferenceOf: each
				onto: aStream]
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeString: aString
onto: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self stringType
		onto: aStream.
	self
		encodeStringBody: aString
		onto: aStream
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeStringBody: aString
onto: aStream [

	| bytes |
	bytes := (aString respondsTo: #utf8Encoded)
		ifTrue: [aString utf8Encoded "pharo"]
		ifFalse: [aString encodeAsUTF8 "GemStone"].
	self
		encodeControlWord: bytes size
		onto: aStream.
	aStream nextPutAll: bytes
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeSymbol: aSymbol
onto: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self symbolType
		onto: aStream.
	self
		encodeStringBody: aSymbol
		onto: aStream
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeTrueOnto: aStream [

	self
		encodeControlWord: self immediateOID
		onto: aStream.
	self
		encodeControlWord: self trueType
		onto: aStream
]

{ #category : #'private-accessing' }
RsrEncoder >> immediateOID [

	^0
]

{ #category : #'private-converting' }
RsrEncoder >> integerAsByteArray: anInteger [
	"Return a ByteArray representing <anInteger> in big endian format."

	| stream int |
	anInteger <= 0
		ifTrue: [^ByteArray with: 0].
	stream := WriteStream on: (ByteArray new: 8).
	int := anInteger.
	[int > 0]
		whileTrue:
			[stream nextPut: (int bitAnd: 16rFF).
			int := int bitShift: -8].
	^stream contents reversed
]

{ #category : #'private-converting' }
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

{ #category : #'private-testing' }
RsrEncoder >> isArray: anObject [

	^anObject class == Array
]

{ #category : #'private-testing' }
RsrEncoder >> isService: anObject [

	^anObject isKindOf: RsrService
]

{ #category : #'private-encoding' }
RsrEncoder >> reflectedVariablesOf: anRsrObject
do: aBlock [

	(self reflectedVariableNamesOf: anRsrObject)
		do:
			[:name |
			aBlock value: (anRsrObject instVarNamed: name)]
]

{ #category : #'private-accessing' }
RsrEncoder >> retainObjectIdentifier [

	^0
]

{ #category : #'private-accessing' }
RsrEncoder >> sendMessageIdentifier [

	^1
]
