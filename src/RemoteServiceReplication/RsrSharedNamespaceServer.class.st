Class {
	#name : #RsrSharedNamespaceServer,
	#superclass : #RsrAbstractSharedNamespace,
	#instVars : [
		'namespace'
	],
	#category : #'RemoteServiceReplication-Public'
}

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

{ #category : #accessing }
RsrSharedNamespaceServer >> rsrConnection: aConnection [

	super rsrConnection: aConnection.
	namespace := aConnection sharedNamespace.
	namespace server: self
]
