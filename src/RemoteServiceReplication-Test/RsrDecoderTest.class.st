Class {
	#name : #RsrDecoderTest,
	#superclass : #RsrCodecTest,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #asserting }
RsrDecoderTest >> assertReference: bytes
decodesTo: expected [

	| actual |
	actual := self decoder decodeObjectReference: bytes readStream.
	self
		assert: actual
		equals: expected
]

{ #category : #decoding }
RsrDecoderTest >> decodeService: anObjectBytes [

	^self decoder decodeService: anObjectBytes readStream
]

{ #category : #running }
RsrDecoderTest >> testDeliverResponse [

	| response encoding command |
	response := #responseSymbol.
	encoding :=
		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"
		#[0 0 0 0 0 0 0 1], "Transaction Id"
		#[0 0 0 0 0 0 0 0], "nil errorName"
		#[0 0 0 0 0 0 0 6], "nil errorName"
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
		assert: command response
		equals: response
]

{ #category : #running }
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

{ #category : #running }
RsrDecoderTest >> testRetainObject [

	| encoding command |
	encoding :=
		#[0 0 0 0 0 0 0 0], "RetainObject Command"
		#[0 0 0 0 0 0 0 0], "ServiceType Object"
		#[0 0 0 0 0 0 0 1], "Service OID"
		#[0 0 0 0 0 0 0 0], "Inst Var Count"
		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 19], "Length of UTF-8 bytes"
		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"
	command := self decoder decodeCommand: encoding readStream.
	self
		assert: command object class
		equals: RsrServerNoInstVars.
	self
		assert: command object _id
		equals: 1.
	self assert: (registry includesKey: command object _id)
]

{ #category : #running }
RsrDecoderTest >> testSendMessage [

	| service encoding command |
	service := RsrServerNoInstVars
		_id: 1
		connection: RsrMockConnection new.
	self decoder.
	service _addTo: registry.
	encoding :=
		#[0 0 0 0 0 0 0 1], "SendMessage Command"
		#[0 0 0 0 0 0 0 1], "Transaction ID"
		#[0 0 0 0 0 0 0 0], "Argument Count"
		#[0 0 0 0 0 0 0 1], "Receiver OID"
		#[0 0 0 0 0 0 0 0], "Selector Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 8], "Length of UTF-8 bytes"
		#[114 101 116 117 114 110 52 50]. "#return42"
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
		equals: #()
]

{ #category : #running }
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

{ #category : #running }
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

{ #category : #running }
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

{ #category : #asserting }
RsrDecoderTest >> verifyControlWord: expected
encoding: bytes [

	| actual |
	actual := self decoder decodeControlWord: bytes readStream.
	self
		assert: actual
		equals: expected
]

{ #category : #asserting }
RsrDecoderTest >> verifyImmediate: expected
encoding: encoding [

	| actual |
	actual := self decoder decodeObjectReference: encoding readStream.
	self
		assert: actual
		equals: expected
]
