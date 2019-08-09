Class {
	#name : #RsrDictionaryElement,
	#superclass : #Object,
	#instVars : [
		'reference'
	],
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #'instance creation' }
RsrDictionaryElement class >> object: anObject [

	^self new
		object: anObject;
		yourself
]

{ #category : #initializing }
RsrDictionaryElement >> initialize [

	super initialize.
	reference := WeakArray new: 1
]

{ #category : #accessing }
RsrDictionaryElement >> object [

	^reference at: 1
]

{ #category : #accessing }
RsrDictionaryElement >> object: anObject [

	reference
		at: 1
		put: anObject
]
