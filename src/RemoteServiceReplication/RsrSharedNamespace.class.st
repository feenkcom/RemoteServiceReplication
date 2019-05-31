Class {
	#name : #RsrSharedNamespace,
	#superclass : #Object,
	#instVars : [
		'map',
		'client',
		'server'
	],
	#category : #'RemoteServiceReplication-Public'
}

{ #category : #adding }
RsrSharedNamespace >> at: aKey [

	^map at: aKey
]

{ #category : #adding }
RsrSharedNamespace >> at: aKey
ifAbsent: aBlock [

	^map at: aKey ifAbsent: aBlock
]

{ #category : #adding }
RsrSharedNamespace >> at: aKey
put: aValue [

	map
		at: aKey
		put: aValue.
	client
		at: aKey
		put: aValue
]

{ #category : #accessing }
RsrSharedNamespace >> client [
	^ client
]

{ #category : #accessing }
RsrSharedNamespace >> client: anObject [
	client := anObject
]

{ #category : #accessing }
RsrSharedNamespace >> map [
	^ map
]

{ #category : #accessing }
RsrSharedNamespace >> map: anObject [
	map := anObject
]

{ #category : #'private-accessing' }
RsrSharedNamespace >> primAt: aKey
put: aValue [

	map
		at: aKey
		put: aValue
]

{ #category : #'private-accessing' }
RsrSharedNamespace >> primRemoveKey: aKey [

	map removeKey: aKey
]

{ #category : #adding }
RsrSharedNamespace >> removeKey: aKey [

	map removeKey: aKey.
	client removeKey: aKey
]

{ #category : #accessing }
RsrSharedNamespace >> server [
	^ server
]

{ #category : #accessing }
RsrSharedNamespace >> server: anObject [
	server := anObject
]
