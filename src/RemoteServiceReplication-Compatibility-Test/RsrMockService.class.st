Class {
	#name : #RsrMockService,
	#superclass : #RsrAbstractService,
	#category : #'RemoteServiceReplication-Compatibility-Test-Support'
}

{ #category : #accessing }
RsrMockService >> _id [

	^1
]

{ #category : #testing }
RsrMockService >> isClient [

	^self class == RsrMockClient
]

{ #category : #testing }
RsrMockService >> isServer [

	^self class == RsrMockServer
]
