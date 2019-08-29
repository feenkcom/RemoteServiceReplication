Class {
	#name : #RsrMockService,
	#superclass : #Object,
	#category : #'RemoteServiceReplication-Compatibility-Test'
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
