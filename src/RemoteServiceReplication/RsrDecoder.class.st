Class {
	#name : #RsrDecoder,
	#superclass : #RsrCodec,
	#instVars : [
		'registry',
		'connection',
		'decodeCommandMap'
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
RsrDecoder >> decodeImmediateObject: aStream [
	| type |
	type := self decodeControlWord: aStream.
	^ (RsrSpecies speciesList at: type + 1)
		decodeReference: aStream
		for: self
]

{ #category : #decoding }
RsrDecoder >> decodeObjectReference: aStream [

	| oid |
	oid := self decodeControlWord: aStream.
	oid = self immediateOID ifTrue: [^self decodeImmediateObject: aStream].
	^registry at: oid ifAbsent: [self signalUnknownOID]
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

	^RsrRetainObject object: (self decodeService: aStream)
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
	| objectType |
	objectType := self decodeControlWord: aStream.
	objectType = self serviceType
		ifTrue: [ ^ RsrServiceSpecies decodeReference: aStream for: self ].
	^ self error: 'Invalid object'
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
