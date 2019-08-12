Class {
	#name : #RsrRegistryElement,
	#superclass : #Object,
	#instVars : [
		'reference'
	],
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #'instance creation' }
RsrRegistryElement class >> object: anObject [

	^self new
		object: anObject;
		yourself
]

{ #category : #initializing }
RsrRegistryElement >> initialize [

	super initialize.
	reference := WeakArray new: 1
]

{ #category : #accessing }
RsrRegistryElement >> object [

	^reference at: 1
]

{ #category : #accessing }
RsrRegistryElement >> object: anObject [

	reference
		at: 1
		put: anObject
]
