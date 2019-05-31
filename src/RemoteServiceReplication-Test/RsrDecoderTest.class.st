Class {
	#name : #RsrDecoderTest,
	#superclass : #RsrCodecTest,
	#instVars : [
		'decoder',
		'registry'
	],
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #asserting }
RsrDecoderTest >> assertControlInteger: expected
encoding: bytes [

	| actual |
	actual := self decoder decodeControlWord: bytes readStream.
	self
		assert: actual
		equals: expected
]

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
RsrDecoderTest >> decode: anObjectBytes [

	^self decoder decodeObject: anObjectBytes readStream
]

{ #category : #accessing }
RsrDecoderTest >> decoder [

	^decoder ifNil: [decoder := RsrDecoder registry: (registry := RsrRegistry new) connection: RsrMockConnection new]
]

{ #category : #running }
RsrDecoderTest >> testArray [

	| encoding actual |
	encoding :=
		#[0 0 0 0 0 0 0 9], "Object Type - Array"
		#[0 0 0 0 0 0 0 1], "OID - 1"
		#[0 0 0 0 0 0 0 5], "size"
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 6], "nil"
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 7], "true"
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 8], "false"
		#[0 0 0 0 0 0 0 1], "self oid"
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 3], "Positive Integer"
		#[0 0 0 0 0 0 0 1], "num bytes in integer"
		#[15].
	actual := self decoder decodeObject: encoding readStream.
	self
		assert: actual class
		equals: RsrArray.
	self
		assert: actual rsrId
		equals: 1.
	self
		assert: (actual at: 1)
		equals: nil.
	self
		assert: (actual at: 2)
		equals: true.
	self
		assert: (actual at: 3)
		equals: false.
	self
		assert: (actual at: 4)
		equals: actual.
	self
		assert: (actual at: 5)
		equals: 15
]

{ #category : #running }
RsrDecoderTest >> testDeliverResponse [

	| response encoding command |
	response := #responseSymbol.
	encoding :=
		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"
		#[0 0 0 0 0 0 0 1], "Transaction Id"
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
RsrDecoderTest >> testRetainNewObject [

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
		assert: command object rsrId
		equals: 1.
	self assert: (registry includes: command object)
]

{ #category : #running }
RsrDecoderTest >> testSendMessage [

	| service encoding command |
	service := RsrServerNoInstVars
		rsrId: 1
		rsrConnection: RsrMockConnection new.
	self decoder.
	service registerWith: registry.
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
	firstService := self decode: self serviceNoInstVarsEncoding.
	secondService := self decode: self serviceNoInstVarsEncoding.
	self
		assert: firstService
		identicalTo: secondService
]

{ #category : #running }
RsrDecoderTest >> testServiceNoInstVars [

	| decodedService |
	decodedService := self decode: self serviceNoInstVarsEncoding.
	self
		assert: decodedService class
		equals: RsrServerNoInstVars.
	self
		assert: decodedService rsrId
		equals: 1
]

{ #category : #running }
RsrDecoderTest >> testServiceReferenceService [

	| rootService referencedService |
	referencedService := self decode: self referencedServiceEncoding.
	self
		assert: referencedService class
		equals: RsrServerNoInstVars.
	self
		assert: referencedService rsrId
		equals: 2.
	rootService := self decode: self rootServiceEncoding.
	self
		assert: rootService class
		equals: RsrServerReferenceService.
	self
		assert: rootService service
		equals: referencedService
]

{ #category : #'running-immediates' }
RsrDecoderTest >> testSymbol [

	self
		assertReference: self genericSymbolEncoding
		decodesTo: self genericSymbol
]
