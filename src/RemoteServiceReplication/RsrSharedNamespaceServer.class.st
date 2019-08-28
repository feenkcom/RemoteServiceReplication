Class {
	#name : #RsrSharedNamespaceServer,
	#superclass : #RsrAbstractSharedNamespace,
	#instVars : [
		'namespace'
	],
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #accessing }
RsrSharedNamespaceServer >> _connection: aConnection [

	super _connection: aConnection.
	namespace := aConnection sharedNamespace.
	namespace server: self
]

{ #category : #accessing }
RsrSharedNamespaceServer >> at: aKey
put: aValue [

	^namespace
		primAt: aKey
		put: aValue
]

{ #category : #accessing }
RsrSharedNamespaceServer >> removeKey: aKey [

	^namespace primRemoveKey: aKey
]
