Class {
	#name : #RsrAbstractSharedNamespace,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Public'
}

{ #category : #accessing }
RsrAbstractSharedNamespace class >> clientClass [

	^RsrSharedNamespaceClient
]

{ #category : #accessing }
RsrAbstractSharedNamespace class >> serverClass [

	^RsrSharedNamespaceServer
]
