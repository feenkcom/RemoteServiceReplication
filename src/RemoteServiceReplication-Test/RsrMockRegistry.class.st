Class {
	#name : 'RsrMockRegistry',
	#superclass : 'RsrObject',
	#instVars : [
		'objects',
		'idSpigot'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'adding' }
RsrMockRegistry >> at: aKey
put: aService [

	self objects add: aService
]

{ #category : 'accessing' }
RsrMockRegistry >> connection [

	^RsrMockConnection new
]

{ #category : 'initializing' }
RsrMockRegistry >> initialize [

	super initialize.
	objects := OrderedCollection new
]

{ #category : 'accessing' }
RsrMockRegistry >> newId [

	^idSpigot next
]

{ #category : 'accessing' }
RsrMockRegistry >> objects [

	^objects
]

{ #category : 'accessing' }
RsrMockRegistry >> objects: anObject [

	objects := anObject
]

{ #category : 'adding' }
RsrMockRegistry >> register: anRsrObject [

	self objects add: anRsrObject
]

{ #category : 'adding' }
RsrMockRegistry >> remember: anRsrObject [

	self objects add: anRsrObject
]

{ #category : 'adding' }
RsrMockRegistry >> retain: anRsrObject [

	self objects add: anRsrObject
]
