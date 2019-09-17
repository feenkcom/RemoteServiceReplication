Class {
	#name : #RsrDecoder,
	#superclass : #RsrCodec,
	#instVars : [
		'registry',
		'connection',
		'decodeCommandMap',
		'decodeImmediateObjectMap'
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

{ #category : #'private-converting' }
RsrDecoder >> bytesAsInteger: bytes [

	| res |
	res := 0.
	bytes do: [:e | res := (res bitShift: 8) bitOr: e].
	^res
]

{ #category : #accessing }
RsrDecoder >> connection [

	^connection
]

{ #category : #accessing }
RsrDecoder >> connection: aConnection [

	connection := aConnection
]

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeArray: aStream [

	| size array |
	size := self decodeControlWord: aStream.
	array := Array new: size.
	1 to: size do: [:i | array at: i put: (self decodeObjectReference: aStream)].
	^array
]

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeByteArray: aStream [

	| size |
	size := self decodeControlWord: aStream.
	^aStream next: size
]

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeCharacter: aStream [

	| codePoint |
	codePoint := self decodeControlWord: aStream.
	^Character codePoint: codePoint
]

{ #category : #'decoding-commands' }
RsrDecoder >> decodeCommand: aStream [
	"Decode an object from the stream"

	| command decodeSelector |
	command := self decodeControlWord: aStream.
	decodeSelector := self decodeCommandMap
		at: command
		ifAbsent: [self error: 'Invalid command received'].
	^self
		perform: decodeSelector
		with: aStream
]

{ #category : #accessing }
RsrDecoder >> decodeCommandMap [

	^decodeCommandMap ifNil: [self initializeDecodeCommandMap]
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

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeDateAndTime: aStream [

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

{ #category : #'decoding-immediates' }
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

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeFalse: aStream [

	^false
]

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeImmediateObject: aStream [

	| type decodeSelector |
	type := self decodeControlWord: aStream.
	decodeSelector := self decodeImmediateObjectMap
		at: type
		ifAbsent: [self error: 'Invalid immediate type specified (', type printString, ')'].
	^self
		perform: decodeSelector
		with: aStream
]

{ #category : #accessing }
RsrDecoder >> decodeImmediateObjectMap [

	^decodeImmediateObjectMap ifNil: [self initializeDecodeImmediateObjectMap]
]

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeNegativeInteger: aStream [

	^(self decodePositiveInteger: aStream) negated
]

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeNil: aStream [

	^nil
]

{ #category : #'decoding-services' }
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

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeOrderedCollection: aStream [

	| size oc |
	size := self decodeControlWord: aStream.
	oc := OrderedCollection new: size.
	size timesRepeat: [oc add: (self decodeObjectReference: aStream)].
	^oc
]

{ #category : #'decoding-immediates' }
RsrDecoder >> decodePositiveInteger: aStream [

	| length bytes |
	length := self decodeControlWord: aStream.
	bytes := aStream next: length.
	^self bytesAsInteger: bytes
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
RsrDecoder >> decodeRetainObject: aStream [

	^RsrRetainObject object: (self decodeObject: aStream)
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

{ #category : #'decoding-services' }
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

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeSet: aStream [

	| size set |
	size := self decodeControlWord: aStream.
	set := Set new: size.
	size timesRepeat: [set add: (self decodeObjectReference: aStream)].
	^set
]

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeString: aStream [

	| length bytes |
	length := self decodeControlWord: aStream.
	bytes := aStream next: length.
	^RsrEnvironment
		ifPharo: [bytes utf8Decoded]
		ifGemStone: [bytes decodeFromUTF8ToString]
		ifDolphin: [(RsrClassResolver classNamed: #Utf8String) fromByteArray: bytes]
]

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeSymbol: aStream [

	^(self decodeString: aStream) asSymbol
]

{ #category : #'decoding-immediates' }
RsrDecoder >> decodeTrue: aStream [

	^true
]

{ #category : #'private-initialization' }
RsrDecoder >> initializeDecodeCommandMap [

	decodeCommandMap := Dictionary new.
	decodeCommandMap
		at: self retainObjectCommand put: #decodeRetainObject:;
		at: self sendMessageCommand put: #decodeSendMessage:;
		at: self deliverResponseCommand put: #decodeDeliverResponse:;
		at: self releaseObjectsCommand put: #decodeReleaseObjects:.
	^decodeCommandMap
]

{ #category : #'private-initialization' }
RsrDecoder >> initializeDecodeImmediateObjectMap [

	decodeImmediateObjectMap := Dictionary new.
	decodeImmediateObjectMap
		at: self symbolType put: #decodeSymbol:;
		at: self stringType put: #decodeString:;
		at: self positiveIntegerType put: #decodePositiveInteger:;
		at: self negativeIntegerType put: #decodeNegativeInteger:;
		at: self characterType put: #decodeCharacter:;
		at: self nilType put: #decodeNil:;
		at: self trueType put: #decodeTrue:;
		at: self falseType put: #decodeFalse:;
		at: self arrayType put: #decodeArray:;
		at: self dictionaryType put: #decodeDictionary:;
		at: self byteArrayType put: #decodeByteArray:;
		at: self setType put: #decodeSet:;
		at: self orderedCollectionType put: #decodeOrderedCollection:;
		at: self dateAndTimeType put: #decodeDateAndTime:.
	^decodeImmediateObjectMap
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
