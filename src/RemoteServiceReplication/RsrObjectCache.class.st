Class {
	#name : 'RsrObjectCache',
	#superclass : 'RsrObject',
	#instVars : [
		'storage'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'adding/removing' }
RsrObjectCache >> add: anObject [

	storage add: anObject
]

{ #category : 'initialization' }
RsrObjectCache >> initialize [

	super initialize.
	self reset
]

{ #category : 'adding/removing' }
RsrObjectCache >> reset [

	storage := IdentitySet new
]
