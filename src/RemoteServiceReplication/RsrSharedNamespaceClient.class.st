Class {
	#name : #RsrSharedNamespaceClient,
	#superclass : #RsrAbstractSharedNamespace,
	#instVars : [
		'mutex',
		'services'
	],
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #registration }
RsrSharedNamespaceClient >> at: aKey
put: aValue [

	^remoteSelf
		at: aKey
		put: aValue
]

{ #category : #registration }
RsrSharedNamespaceClient >> removeKey: aKey [

	^remoteSelf removeKey: aKey
]
