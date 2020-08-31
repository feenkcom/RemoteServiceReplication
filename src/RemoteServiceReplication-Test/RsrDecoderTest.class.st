Class {
	#name : 'RsrDecoderTest',
	#superclass : 'RsrCodecTest',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'asserting' }
RsrDecoderTest >> assertReference: bytes
decodesTo: expected [

	| actual |
	actual := (self decoder decodeReference: bytes readStream) resolve: self decoder registry.
	self
		assert: actual
		equals: expected
]

{ #category : 'decoding' }
RsrDecoderTest >> decodeService: anObjectBytes [

	^(self decoder decodeServiceSnapshot: anObjectBytes readStream) reifyIn: self decoder connection
]

{ #category : 'running' }
RsrDecoderTest >> testDeliverErrorResponse [

	| encoding command error |
	encoding :=
		#[0 0 0 0 0 0 0 4], "DeliverErrorResponse Command"
		#[0 0 0 0 0 0 0 1], "Transaction Id"
		#[0 0 0 0 0 0 0 0], "originalClassName: Symbol encoding"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 5], "Length of UTF-8 bytes"
		#[69 114 114 111 114], "#Error"
		#[0 0 0 0 0 0 0 0], "OID = 0"
		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"
		#[0 0 0 0 0 0 0 3], "length"
		#[116 97 103],	 "tag"
		#[0 0 0 0 0 0 0 0], "OID = 0"
		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"
		#[0 0 0 0 0 0 0 12], "length"
		#[109 101 115 115 97 103 101 32 116 101 120 116],	 "message text"
		#[0 0 0 0 0 0 0 0], "OID = 0"
		#[0 0 0 0 0 0 0 2], "Immediate Type = 2"
		#[0 0 0 0 0 0 0 10], "length"
		#[115 116 97 99 107 32 100 117 109 112].	 "stack dump"
	command := self decoder decodeCommand: encoding readStream.
	self
		assert: command class
		equals: RsrDeliverErrorResponse.
	self
		assert: command transaction
		equals: 1.
	error := command remoteError.
	self
		assert: error class
		equals: RsrRemoteError.
	self
		assert: error originalClassName
		equals: #Error.
	self
		assert: error tag
		equals: 'tag'.
	self
		assert: error messageText
		equals: 'message text'.
	self
		assert: error stack
		equals: 'stack dump'
]

{ #category : 'running' }
RsrDecoderTest >> testDeliverResponse [

	| service response encoding command |
	service := RsrServerNoInstVars
		_id: 1
		connection: RsrMockConnection new.
	self decoder.
	self registry
		serviceAt: 1
		put: service.
	response := #responseSymbol.
	encoding :=
		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"
		#[0 0 0 0 0 0 0 1], "Transaction Id"
		#[0 0 0 0 0 0 0 1], "Number of services"
		self serviceNoInstVarsEncoding,
		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 14], "Length of UTF-8 bytes"
		#[114 101 115 112 111 110 115 101 83 121 109 98 111 108]. "#responseSymbol"
	command := self decoder decodeCommand: encoding readStream.
	self
		assert: command class
		equals: RsrDeliverResponse.
	self
		assert: command transaction
		equals: 1.
	self
		assert: command services
		equals: (Array with: service).
	self
		assert: command response
		equals: response
]

{ #category : 'running' }
RsrDecoderTest >> testReleaseObjects [

	| command encoding |
	encoding :=
		#[0 0 0 0 0 0 0 3], "ReleaseObjects Command"
		#[0 0 0 0 0 0 0 5], "Num OIDS"
		#[0 0 0 0 0 0 0 1], "First OID"
		#[0 0 0 0 0 0 0 2],
		#[0 0 0 0 0 0 0 3],
		#[0 0 0 0 0 0 0 4],
		#[0 0 0 0 0 0 0 5]. "Last OID"
	command := self decoder decodeCommand: encoding readStream.
	self
		assert: command oids
		equals: #(1 2 3 4 5)
]

{ #category : 'running' }
RsrDecoderTest >> testSendMessage [

	| service encoding command |
	service := RsrServerNoInstVars
		_id: 1
		connection: self connection.
	self decoder.
	self registry
		serviceAt: 1
		put: service.
	encoding :=
		#[0 0 0 0 0 0 0 1], "SendMessage Command"
		#[0 0 0 0 0 0 0 1], "Transaction ID"
		#[0 0 0 0 0 0 0 1], "One service is part of this message"
		self serviceNoInstVarsEncoding,
		#[0 0 0 0 0 0 0 1], "Receiver OID"
		#[0 0 0 0 0 0 0 0], "Selector Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 8], "Length of UTF-8 bytes"
		#[114 101 116 117 114 110 52 50], "#return42"
		#[0 0 0 0 0 0 0 0]. "Argument Count"
	command := self decoder decodeCommand: encoding readStream.
	self
		assert: command class
		equals: RsrSendMessage.
	self
		assert: command transaction
		equals: 1.
	self
		assert: command receiver
		identicalTo: service.
	self
		assert: command selector
		identicalTo: #return42.
	self
		assert: command arguments
		equals: #().
	self
		assert: command services
		equals: (Array with: service)
]

{ #category : 'running' }
RsrDecoderTest >> testServiceDecodeIdentity [
	"Ensure that decoding an object multiple times results in
	a single object getting created."

	| firstService secondService |
	firstService := self decodeService: self serviceNoInstVarsEncoding.
	secondService := self decodeService: self serviceNoInstVarsEncoding.
	self
		assert: firstService
		identicalTo: secondService
]

{ #category : 'running' }
RsrDecoderTest >> testServiceNoInstVars [

	| decodedService |
	decodedService := self decodeService: self serviceNoInstVarsEncoding.
	self
		assert: decodedService class
		equals: RsrServerNoInstVars.
	self
		assert: decodedService _id
		equals: 1
]

{ #category : 'running' }
RsrDecoderTest >> testServiceReferenceService [

	| rootService referencedService |
	referencedService := self decodeService: self referencedServiceEncoding.
	self
		assert: referencedService class
		equals: RsrServerNoInstVars.
	self
		assert: referencedService _id
		equals: 2.
	rootService := self decodeService: self rootServiceEncoding.
	self
		assert: rootService class
		equals: RsrServerReferenceService.
	self
		assert: rootService service
		equals: referencedService
]

{ #category : 'asserting' }
RsrDecoderTest >> verifyControlWord: expected
encoding: bytes [

	| actual |
	actual := self decoder decodeControlWord: bytes readStream.
	self
		assert: actual
		equals: expected
]

{ #category : 'asserting' }
RsrDecoderTest >> verifyImmediate: expected
encoding: encoding [

	| actual |
	actual := (self decoder decodeReference: encoding readStream) resolve: self decoder registry.
	self
		assert: actual
		equals: expected
]
