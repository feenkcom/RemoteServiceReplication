Class {
	#name : #RsrDecoder,
	#superclass : #RsrCodec,
	#instVars : [
		'registry',
		'connection'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrDecoder class >> registry: anRsrRegistry
connection: aConnection [

	^self new
		registry: anRsrRegistry;
		connection: aConnection;
		yourself
]

{ #category : #'private-decoding' }
RsrDecoder >> bytesAsInteger: bytes [

	^bytes
		inject: 0
		into: [:int :byte | (int bitShift: 8) bitOr: byte]
]

{ #category : #accessing }
RsrDecoder >> connection [

	^connection
]

{ #category : #accessing }
RsrDecoder >> connection: aConnection [

	connection := aConnection
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeArray: aStream [

	| size array |
	size := self decodeControlWord: aStream.
	array := Array new: size.
	1 to: size do: [:i | array at: i put: (self decodeObjectReference: aStream)].
	^array
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeByteArray: aStream [

	| size |
	size := self decodeControlWord: aStream.
	^aStream next: size
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeCharacter: aStream [

	| codePoint |
	codePoint := self decodeControlWord: aStream.
	^Character codePoint: codePoint
]

{ #category : #'decoding-commands' }
RsrDecoder >> decodeCommand: aStream [
	"Decode an object from the stream"

	| command |
	command := self decodeControlWord: aStream.
	command == self retainObjectCommand
		ifTrue: [^RsrRetainObject object: (self decodeObject: aStream)].
	command == self sendMessageCommand
		ifTrue: [^self decodeSendMessage: aStream].
	command == self deliverResponseCommand
		ifTrue: [^self decodeDeliverResponse: aStream].
	command == self releaseObjectsCommand
		ifTrue: [^self decodeReleaseObjects: aStream].
	self error: 'Invalid message'
]

{ #category : #decoding }
RsrDecoder >> decodeControlWord: aStream [

	| bytes unsignedResult |
	bytes := aStream next: self sizeOfInteger.
	unsignedResult := self bytesAsInteger: bytes.
	^unsignedResult > self controlWordMax
		ifTrue: [(2 raisedTo: 64) negated + unsignedResult]
		ifFalse: [unsignedResult]
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeDateTime: aStream [

	| microseconds |
	microseconds := self decodeControlWord: aStream.
	^RsrDateAndTimeInterface fromMicroseconds: microseconds
]

{ #category : #'decoding-commands' }
RsrDecoder >> decodeDeliverResponse: aStream [

	| transaction errorName response |
	transaction := self decodeControlWord: aStream.
	errorName := self decodeObjectReference: aStream.
	response := self decodeObjectReference: aStream.
	^RsrDeliverResponse new
		transaction: transaction;
		errorName: errorName;
		response: response;
		yourself
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeDictionary: aStream [

	| size dictionary |
	size := self decodeControlWord: aStream.
	dictionary := Dictionary new: size.
	1
		to: size
		do:
			[:i |
			dictionary
				at: (self decodeObjectReference: aStream)
				put: (self decodeObjectReference: aStream)].
	^dictionary
]

{ #category : #decoding }
RsrDecoder >> decodeImmediateInteger: aStream [

	| length bytes |
	length := self decodeControlWord: aStream.
	bytes := aStream next: length.
	^self bytesAsInteger: bytes
]

{ #category : #decoding }
RsrDecoder >> decodeImmediateObject: aStream [

	| type |
	type := self decodeControlWord: aStream.
	type = self symbolType
		ifTrue: [^self decodeSymbol: aStream].
	type = self stringType
		ifTrue: [^self decodeString: aStream].
	type = self positiveIntegerType
		ifTrue: [^self decodeImmediateInteger: aStream].
	type = self negativeIntegerType
		ifTrue: [^self decodeNegativeInteger: aStream].
	type = self characterType
		ifTrue: [^self decodeCharacter: aStream].
	type = self nilType
		ifTrue: [^nil].
	type = self trueType
		ifTrue: [^true].
	type = self falseType
		ifTrue: [^false].
	type = self arrayType
		ifTrue: [^self decodeArray: aStream].
	type = self dictionaryType
		ifTrue: [^self decodeDictionary: aStream].
	type = self byteArrayType
		ifTrue: [^self decodeByteArray: aStream].
	type = self setType
		ifTrue: [^self decodeSet: aStream].
	type = self orderedCollectionType
		ifTrue: [^self decodeOrderedCollection: aStream].
	type = self dateTimeType
		ifTrue: [^self decodeDateTime: aStream].
	self error: 'Invalid immediate type specified (', type printString, ')'
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeInteger: aStream [

	| length bytes |
	length := self decodeControlWord: aStream.
	bytes := aStream next: length.
	^bytes asInteger
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeNegativeInteger: aStream [

	^(self decodeImmediateInteger: aStream) negated
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeObject: aStream [

	| objectType |
	objectType := self decodeControlWord: aStream.
	objectType = self serviceType
		ifTrue: [^self decodeService: aStream].
	^self error: 'Invalid object'
]

{ #category : #decoding }
RsrDecoder >> decodeObjectReference: aStream [

	| oid |
	oid := self decodeControlWord: aStream.
	oid = 0 ifTrue: [^self decodeImmediateObject: aStream].
	^registry at: oid ifAbsent: [self signalUnknownOID]
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeOrderedCollection: aStream [

	| size oc |
	size := self decodeControlWord: aStream.
	oc := OrderedCollection new: size.
	size timesRepeat: [oc add: (self decodeObjectReference: aStream)].
	^oc
]

{ #category : #'decoding-commands' }
RsrDecoder >> decodeReleaseObjects: aStream [

	| count oids |
	count := self decodeControlWord: aStream.
	oids := Array new: count.
	1
		to: count
		do:
			[:i | | oid |
			oid := self decodeControlWord: aStream.
			oids at: i put: oid].
	^RsrReleaseObjects oids: oids
]

{ #category : #'decoding-commands' }
RsrDecoder >> decodeSendMessage: aStream [

	| transaction argCount receiverOID receiver selector arguments |
	transaction := self decodeControlWord: aStream.
	argCount := self decodeControlWord: aStream.
	receiverOID := self decodeControlWord: aStream.
	receiver := registry at: receiverOID ifAbsent: [^self signalUnknownOID].
	selector := self decodeObjectReference: aStream.
	arguments := (1 to: argCount) collect: [:each | self decodeObjectReference: aStream].
	^RsrSendMessage
		transaction: transaction
		receiver: receiver
		selector: selector
		arguments: arguments
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeService: aStream [

	| oid instVarCount serviceName instance |
	oid := self decodeControlWord: aStream.
	instVarCount := self decodeControlWord: aStream.
	serviceName := self decodeObjectReference: aStream.
	instance := registry
		at: oid
		ifAbsent:
			[((self lookupClass: serviceName)
				_id: oid
				_connection: self connection)
					yourself].
	instance registerWith: self registry.
	(RsrReflection reflectedVariableNamesOf: instance) size = instVarCount
		ifFalse: [self error: 'Incorrectly encoded instance detected'].
	RsrReflection
		reflectedVariableIndecesOf: instance
		do: [:index | instance instVarAt: index put: (self decodeObjectReference: aStream)].
	^instance
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeSet: aStream [

	| size set |
	size := self decodeControlWord: aStream.
	set := Set new: size.
	size timesRepeat: [set add: (self decodeObjectReference: aStream)].
	^set
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeString: aStream [

	| length bytes |
	length := self decodeControlWord: aStream.
	bytes := aStream next: length.
	^RsrEnvironment
		ifPharo: [bytes utf8Decoded]
		ifGemStone: [bytes decodeFromUTF8ToString]
]

{ #category : #'decoding-objects' }
RsrDecoder >> decodeSymbol: aStream [

	^(self decodeString: aStream) asSymbol
]

{ #category : #utility }
RsrDecoder >> lookupClass: aClassName [

	^RsrClassResolver classNamed: aClassName
]

{ #category : #accessing }
RsrDecoder >> registry [

	^registry
]

{ #category : #accessing }
RsrDecoder >> registry: anRsrRegistry [

	registry := anRsrRegistry
]

{ #category : #'error handling' }
RsrDecoder >> signalUnknownOID [

	RsrUnknownOID signal
]
