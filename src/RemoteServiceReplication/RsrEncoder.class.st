Class {
	#name : #RsrEncoder,
	#superclass : #RsrCodec,
	#category : #'RemoteServiceReplication-Communications'
}

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
		ifTrue: [^self encodeService: anObject onto: aStream].
	RsrUnsupportedObject signal: anObject
]

{ #category : #'private-encoding' }
RsrEncoder >> encodeReferenceOf: anObject
onto: aStream [

	| species |
	species := self speciesOf: anObject.
	species
		encodeReference: anObject
		with: self
		onto: aStream
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
onto: aStream [

	"type"
	"the OID for the object"
	"the name of the remote service to create"
	"Write the object slots"
	| reflectedVariables |
	reflectedVariables := aService variablesToReflect.
	self
		encodeControlWord: self serviceType
		onto: aStream.
	self
		encodeControlWord: aService _id
		onto: aStream.
	self
		encodeControlWord: reflectedVariables size
		onto: aStream.
	(self speciesOf: aService remoteServiceName)
		encodeReference: aService remoteServiceName
		with: self
		onto: aStream.
	aService reflectedVariablesDo: [:each | self encodeReferenceOf: each onto: aStream]
]

{ #category : #converting }
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
RsrEncoder >> isImmediate: anObject [

	^self speciesMapping includesKey: anObject class
]

{ #category : #'private-testing' }
RsrEncoder >> isService: anObject [

	^anObject isKindOf: RsrService
]

{ #category : #'private-accessing' }
RsrEncoder >> retainObjectIdentifier [

	^0
]

{ #category : #'private-accessing' }
RsrEncoder >> sendMessageIdentifier [

	^1
]

{ #category : #'private-accessing' }
RsrEncoder >> speciesMapping [
	"Return a mapping between the native class and their associated RsrSpecies"

	^RsrSpecies speciesMapping
]

{ #category : #accessing }
RsrEncoder >> speciesOf: anObject [

	(self isService: anObject)
		ifTrue: [^RsrServiceSpecies].
	anObject == true
		ifTrue: [^RsrTrueSpecies].
	anObject == false
		ifTrue: [^RsrFalseSpecies].
	(anObject isKindOf: Integer)
		ifTrue: [^anObject positive ifTrue: [RsrPositiveIntegerSpecies] ifFalse: [RsrNegativeIntegerSpecies]].
	^self speciesMapping
		at: anObject class
		ifAbsent: [RsrSpecies nullSpecies]
]
