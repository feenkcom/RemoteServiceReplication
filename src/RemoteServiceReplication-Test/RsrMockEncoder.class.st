Class {
	#name : 'RsrMockEncoder',
	#superclass : 'RsrEncoder',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'encoding' }
RsrMockEncoder >> encode: anObject [

	^nil
]

{ #category : 'encoding' }
RsrMockEncoder >> encodeObject: anObject [

	^ByteArray new
]
