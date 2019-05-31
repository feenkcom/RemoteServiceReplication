Class {
	#name : #RsrCodecTest,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #testing }
RsrCodecTest class >> isAbstract [

	^self == RsrCodecTest
]

{ #category : #'running-symbol' }
RsrCodecTest >> genericSymbol [

	^#genericSymbol
]

{ #category : #'running-symbol' }
RsrCodecTest >> genericSymbolEncoding [

	^#[0 0 0 0 0 0 0 0], "OID = 0"
	#[0 0 0 0 0 0 0 1], "Immediate Type = 1"
	#[0 0 0 0 0 0 0 13], "Length of UTF-8 data"
	#[103 101 110 101 114 105 99 83 121 109 98 111 108]	"#genericSymbol"
]

{ #category : #'accessing-objects' }
RsrCodecTest >> referencedServiceEncoding [

	^#[0 0 0 0 0 0 0 0], "type"
	#[0 0 0 0 0 0 0 2], "referencedService's OID = 2"
	#[0 0 0 0 0 0 0 0], "Inst Var Count"
	#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"
	#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"
	#[0 0 0 0 0 0 0 19], "Length of UTF-8 encoded bytes"
	#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"
]

{ #category : #'accessing-objects' }
RsrCodecTest >> rootServiceEncoding [

	^#[0 0 0 0 0 0 0 0], "type"
	#[0 0 0 0 0 0 0 1], "rootService's OID = 1"
	#[0 0 0 0 0 0 0 1], "Inst Var Count"
	#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"
	#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"
	#[0 0 0 0 0 0 0 25], "Length of UTF-8 encoded bytes"
	#[82 115 114 83 101 114 118 101 114 82 101 102 101 114 101 110 99 101 83 101 114 118 105 99 101],
	#[0 0 0 0 0 0 0 2]. "#RsrServerReferenceService"
]

{ #category : #running }
RsrCodecTest >> serviceNoInstVarsEncoding [

	^#[0 0 0 0 0 0 0 0], "type"
	#[0 0 0 0 0 0 0 1], "rootService's OID = 1"
	#[0 0 0 0 0 0 0 0], "Inst Var Count"
	#[0 0 0 0 0 0 0 0], "Start of service name. OID = 0"
	#[0 0 0 0 0 0 0 1], "Service name = 1 -> Symbol"
	#[0 0 0 0 0 0 0 19], "Length of UTF-8 encoded bytes"
	#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115] "#RsrServerNoInstVars"
]

{ #category : #'running-immediates' }
RsrCodecTest >> testArray [

	| array encoding |
	array := Array
		with: self genericSymbol
		with: 5
		with: nil.
	encoding :=
		#[0 0 0 0 0 0 0 0], "Immediate Object OID"
		#[0 0 0 0 0 0 0 9], "Array type"
		#[0 0 0 0 0 0 0 3], "3 elements"
		self genericSymbolEncoding, "Generic Symbol"
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 3], "Positive Integer"
		#[0 0 0 0 0 0 0 1], "num bytes"
		#[5], "5"
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 6].
	self
		verifyImmediate: array
		encoding: encoding.
	array := Array new.
	encoding :=
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 9], "Array type"
		#[0 0 0 0 0 0 0 0].
	self
		verifyImmediate: array
		encoding: encoding
]

{ #category : #'running-control words' }
RsrCodecTest >> testControlWord [
	
	self
		verifyControlWord: 0
		encoding: #[0 0 0 0 0 0 0 0].
	self
		verifyControlWord: 1
		encoding: #[0 0 0 0 0 0 0 1].
	self
		verifyControlWord: -1
		encoding: #[255 255 255 255 255 255 255 255].
	self
		verifyControlWord: (2 raisedTo: 63) - 1
		encoding: #[127 255 255 255 255 255 255 255].
	self
		verifyControlWord: (2 raisedTo: 63) negated
		encoding: #[128 0 0 0 0 0 0 0]
]

{ #category : #'running-symbol' }
RsrCodecTest >> testSymbol [

	self
		verifyImmediate: self genericSymbol
		encoding: self genericSymbolEncoding
]

{ #category : #asserting }
RsrCodecTest >> verifyControlWord: anInteger
encoding: bytes [

	self subclassResponsibility
]

{ #category : #asserting }
RsrCodecTest >> verifyImmediate: anImmediateObject
encoding: encoding [

	self subclassResponsibility
]
