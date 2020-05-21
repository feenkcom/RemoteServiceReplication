Class {
	#name : #RsrRegistryEntry,
	#superclass : #RsrObject,
	#instVars : [
		'storage'
	],
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #'instance creation' }
RsrRegistryEntry class >> value: anObject [

	^self new
		value: anObject;
		yourself
]

{ #category : #initializing }
RsrRegistryEntry >> initialize [

	super initialize.
	storage := self storageClass new: 1
]

{ #category : #accessing }
RsrRegistryEntry >> service [

	^storage at: 1
]

{ #category : #accessing }
RsrRegistryEntry >> service: aService [

	storage
		at: 1
		put: aService
]

{ #category : #accessing }
RsrRegistryEntry >> storageClass [
	"Return the class of the storage"

	^Array
]

{ #category : #accessing }
RsrRegistryEntry >> value [

	^storage at: 1
]

{ #category : #accessing }
RsrRegistryEntry >> value: anObject [

	storage
		at: 1
		put: anObject
]
