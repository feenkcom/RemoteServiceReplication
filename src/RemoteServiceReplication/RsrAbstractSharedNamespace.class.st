Class {
	#name : #RsrAbstractSharedNamespace,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #accessing }
RsrAbstractSharedNamespace class >> clientClass [

	^RsrSharedNamespaceClient
]

{ #category : #accessing }
RsrAbstractSharedNamespace class >> serverClass [

	^RsrSharedNamespaceServer
]
