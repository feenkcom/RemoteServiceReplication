Class {
	#name : 'RsrDecoder',
	#superclass : 'RsrCodec',
	#instVars : [
		'registry',
		'connection',
		'decodeCommandMap'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrDecoder class >> registry: anRsrRegistry
connection: aConnection [

	^self new
		registry: anRsrRegistry;
		connection: aConnection;
		yourself
]

{ #category : 'private-converting' }
RsrDecoder >> bytesAsInteger: bytes [

	| res |
	res := 0.
	bytes do: [:e | res := (res bitShift: 8) bitOr: e].
	^res
]

{ #category : 'accessing' }
RsrDecoder >> connection [

	^connection
]

{ #category : 'accessing' }
RsrDecoder >> connection: aConnection [

	connection := aConnection
]

{ #category : 'decoding' }
RsrDecoder >> decodeAndResolveObjectReference: aStream [

	| oid |
	oid := self decodeControlWord: aStream.
	oid = self immediateOID ifTrue: [^self decodeImmediateObject: aStream].
	^registry serviceAt: oid ifAbsent: [self signalUnknownOID]
]

{ #category : 'decoding-commands' }
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

{ #category : 'accessing' }
RsrDecoder >> decodeCommandMap [

	^decodeCommandMap ifNil: [self initializeDecodeCommandMap]
]

{ #category : 'decoding' }
RsrDecoder >> decodeControlWord: aStream [

	| bytes unsignedResult |
	bytes := aStream next: self sizeOfInteger.
	unsignedResult := self bytesAsInteger: bytes.
	^unsignedResult > self controlWordMax
		ifTrue: [(2 raisedTo: 64) negated + unsignedResult]
		ifFalse: [unsignedResult]
]

{ #category : 'decoding-commands' }
RsrDecoder >> decodeDeliverErrorResponse: aStream [

	| transaction originalClassName tag messageText stack error |
	transaction := self decodeControlWord: aStream.
	originalClassName := self decodeAndResolveObjectReference: aStream.
	tag := self decodeAndResolveObjectReference: aStream.
	messageText := self decodeAndResolveObjectReference: aStream.
	stack := self decodeAndResolveObjectReference: aStream.
	error := RsrRemoteError new
		originalClassName: originalClassName;
		tag: tag;
		messageText: messageText;
		stack: stack;
		yourself.
	^RsrDeliverErrorResponse new
		transaction: transaction;
		remoteError: error;
		yourself
]

{ #category : 'decoding-commands' }
RsrDecoder >> decodeDeliverResponse: aStream [

    | transaction numServices services response |
    transaction := self decodeControlWord: aStream.
    numServices := self decodeControlWord: aStream.
    services := (1 to: numServices) collect: [:each | self decodeService: aStream].
    response := self decodeAndResolveObjectReference: aStream.
    ^RsrDeliverResponse new
        transaction: transaction;
        services: services;
        response: response;
        yourself
]

{ #category : 'decoding-services' }
RsrDecoder >> decodeImmediateObject: aStream [

	| species |
	species := self decodeControlWord: aStream.
	^(RsrSpecies speciesList at: species + 1)
		decodeReference: aStream
		using: self
]

{ #category : 'decoding' }
RsrDecoder >> decodeObjectReference: aStream [

	| oid |
	oid := self decodeControlWord: aStream.
	oid = self immediateOID ifTrue: [^RsrValueReference value: (self decodeImmediateObject: aStream)].
	^RsrServiceReference sid: oid
]

{ #category : 'decoding-commands' }
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

{ #category : 'decoding-commands' }
RsrDecoder >> decodeSendMessage: aStream [

	| transaction argCount receiverReference receiver selector numServices services arguments instance |
	transaction := self decodeControlWord: aStream.
	numServices := self decodeControlWord: aStream.
	services := (1 to: numServices) collect: [:each | self decodeService: aStream].
	receiverReference := self decodeObjectReference: aStream.
	receiver := receiverReference resolve: self registry.
	selector := self decodeAndResolveObjectReference: aStream.
	argCount := self decodeControlWord: aStream.
	arguments := (1 to: argCount) collect: [:each | self decodeAndResolveObjectReference: aStream].
	instance := RsrSendMessage
		transaction: transaction
		receiver: receiver
		selector: selector
		arguments: arguments.
	instance services: services.
	^instance
]

{ #category : 'decoding-services' }
RsrDecoder >> decodeService: aStream [

	^RsrServiceSpecies
		decode: aStream
		using: self
]

{ #category : 'private-initialization' }
RsrDecoder >> initializeDecodeCommandMap [

	decodeCommandMap := Dictionary new.
	decodeCommandMap
		at: self sendMessageCommand put: #decodeSendMessage:;
		at: self deliverResponseCommand put: #decodeDeliverResponse:;
		at: self releaseObjectsCommand put: #decodeReleaseObjects:;
		at: self deliverErrorResponseCommand put: #decodeDeliverErrorResponse:.
	^decodeCommandMap
]

{ #category : 'utility' }
RsrDecoder >> lookupClass: aClassName [

	^RsrClassResolver classNamed: aClassName
]

{ #category : 'accessing' }
RsrDecoder >> registry [

	^registry
]

{ #category : 'accessing' }
RsrDecoder >> registry: anRsrRegistry [

	registry := anRsrRegistry
]

{ #category : 'error handling' }
RsrDecoder >> signalUnknownOID [

	RsrUnknownOID signal
]
