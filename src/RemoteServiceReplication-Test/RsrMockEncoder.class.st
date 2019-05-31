Class {
	#name : #RsrMockEncoder,
	#superclass : #RsrCodec,
	#category : 'RemoteServiceReplication-Test-Mocks'
}

{ #category : #encoding }
RsrMockEncoder >> encode: anObject [

	^RsrRetainObject
		object: anObject
		encoding: ByteArray new
]

{ #category : #encoding }
RsrMockEncoder >> encodeObject: anObject [

	^ByteArray new
]

{ #category : #encoding }
RsrMockEncoder >> encodeRetainObject: anObject [

	^ByteArray new
]
