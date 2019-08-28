Class {
	#name : #RsrEncoderTest,
	#superclass : #RsrCodecTest,
	#instVars : [
		'connection'
	],
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #registration }
RsrEncoderTest >> register: aService [

	aService
		_id: connection oidSpigot next;
		_connection: connection.
	registry retain: aService
]

{ #category : #running }
RsrEncoderTest >> setUp [

	super setUp.
	registry := RsrMockRegistry new.
	connection := RsrMockConnection new
]

{ #category : #running }
RsrEncoderTest >> tearDown [

	registry := connection := nil.
	super setUp
]

{ #category : #'running-immediates' }
RsrEncoderTest >> testDeliverResponse [

	| response command expectedEncoding |
	response := #responseSymbol.
	command := RsrDeliverResponse
		transaction: 1
		response: response.
	command encodeUsing: RsrEncoder new.
	expectedEncoding :=
		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"
		#[0 0 0 0 0 0 0 1], "Transaction Id"
		#[0 0 0 0 0 0 0 0], "nil errorName"
		#[0 0 0 0 0 0 0 6], "nil errorName"
		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 14], "Length of UTF-8 bytes"
		#[114 101 115 112 111 110 115 101 83 121 109 98 111 108]. "#responseSymbol"
	self
		assert: command encoding
		equals: expectedEncoding
]

{ #category : #'running-immediates' }
RsrEncoderTest >> testReleaseObjects [

	| command expectedEncoding |
	command := RsrReleaseObjects oids: #(1 2 3 4 5).
	command encodeUsing: RsrEncoder new.
	expectedEncoding :=
		#[0 0 0 0 0 0 0 3], "ReleaseObjects Command"
		#[0 0 0 0 0 0 0 5], "Num OIDS"
		#[0 0 0 0 0 0 0 1], "First OID"
		#[0 0 0 0 0 0 0 2],
		#[0 0 0 0 0 0 0 3],
		#[0 0 0 0 0 0 0 4],
		#[0 0 0 0 0 0 0 5]. "Last OID"
	self
		assert: command encoding
		equals: expectedEncoding
]

{ #category : #'running-immediates' }
RsrEncoderTest >> testRetainObject [

	| service command expectedEncoding |
	service := RsrClientNoInstVars new.
	self register: service.
	command := RsrRetainObject object: service.
	command encodeUsing: RsrEncoder new.
	expectedEncoding :=
		#[0 0 0 0 0 0 0 0], "RetainObject Command"
		#[0 0 0 0 0 0 0 0], "ServiceType Object"
		#[0 0 0 0 0 0 0 1], "Service OID"
		#[0 0 0 0 0 0 0 0], "Inst Var Count"
		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 19], "Length of UTF-8 bytes"
		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"
	self
		assert: command encoding
		equals: expectedEncoding
]

{ #category : #'running-immediates' }
RsrEncoderTest >> testSendMessage [

	| service command expectedEncoding |
	service := RsrClientNoInstVars new.
	self register: service.
	command := RsrSendMessage
		transaction: 1
		receiver: service
		selector: #return42
		arguments: #().
	command encodeUsing: RsrEncoder new.
	expectedEncoding :=
		#[0 0 0 0 0 0 0 1], "SendMessage Command"
		#[0 0 0 0 0 0 0 1], "Transaction ID"
		#[0 0 0 0 0 0 0 0], "Argument Count"
		#[0 0 0 0 0 0 0 1], "Receiver OID"
		#[0 0 0 0 0 0 0 0], "Selector Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 8], "Length of UTF-8 bytes"
		#[114 101 116 117 114 110 52 50]. "#return42"
	self
		assert: command encoding
		equals: expectedEncoding
]

{ #category : #running }
RsrEncoderTest >> testServiceNoInstVars [

	| rootService encodedBytes expectedEncoding |
	rootService := RsrClientNoInstVars new.
	self register: rootService.
	encodedBytes := self encoder encodeObject: rootService.
	expectedEncoding := self serviceNoInstVarsEncoding.
	self
		assert: encodedBytes
		equals: expectedEncoding
]

{ #category : #running }
RsrEncoderTest >> testServiceReferenceService [

	| rootService referencedService encodedObject expectedEncoding |
	referencedService := RsrClientNoInstVars new.
	rootService := RsrClientReferenceService service: referencedService.
	self
		register: rootService;
		register: referencedService.
	encodedObject := self encoder encodeObject: rootService.
	expectedEncoding :=
		#[0 0 0 0 0 0 0 0], "type"
		#[0 0 0 0 0 0 0 1], "rootService's OID = 1"
		#[0 0 0 0 0 0 0 1], "Inst Var Count"
		#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"
		#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"
		#[0 0 0 0 0 0 0 25], "Length of UTF-8 encoded bytes"
		#[82 115 114 83 101 114 118 101 114 82 101 102 101 114 101 110 99 101 83 101 114 118 105 99 101],
		#[0 0 0 0 0 0 0 2]. "#RsrServerReferenceService"
	self
		assert: encodedObject
		equals: expectedEncoding.
	encodedObject := self encoder encodeObject: referencedService.
	expectedEncoding :=
		#[0 0 0 0 0 0 0 0], "type"
		#[0 0 0 0 0 0 0 2], "referencedService's OID = 2"
		#[0 0 0 0 0 0 0 0], "Inst Var Count"
		#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"
		#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"
		#[0 0 0 0 0 0 0 19], "Length of UTF-8 encoded bytes"
		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"
	self
		assert: encodedObject
		equals: expectedEncoding
]

{ #category : #'running-immediates' }
RsrEncoderTest >> verifyControlWord: anInteger
encoding: expected [

	| actual |
	actual := ByteArray streamContents: [:stream | self encoder encodeControlWord: anInteger onto: stream].
	self
		assert: actual
		equals: expected
]

{ #category : #'running-immediates' }
RsrEncoderTest >> verifyImmediate: anObject
encoding: expected [

	| actual |
	actual := ByteArray streamContents: [:stream | self encoder encodeReferenceOf: anObject onto: stream].
	self
		assert: actual
		equals: expected
]
