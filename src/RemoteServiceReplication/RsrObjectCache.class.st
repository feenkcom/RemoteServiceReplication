Class {
	#name : #RsrObjectCache,
	#superclass : #Object,
	#instVars : [
		'storage'
	],
	#category : 'RemoteServiceReplication-Utilities'
}

{ #category : #'adding/removing' }
RsrObjectCache >> add: anObject [

	storage add: anObject
]

{ #category : #initialization }
RsrObjectCache >> initialize [

	super initialize.
	self reset
]

{ #category : #'adding/removing' }
RsrObjectCache >> reset [

	storage := IdentitySet new
]
