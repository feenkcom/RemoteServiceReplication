Class {
	#name : #RsrRegistryElement,
	#superclass : #RsrObject,
	#instVars : [
		'storage'
	],
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #'instance creation' }
RsrRegistryElement class >> value: anObject [

	^self new
		value: anObject;
		yourself
]

{ #category : #initializing }
RsrRegistryElement >> initialize [

	super initialize.
	storage := self storageClass new: 1
]

{ #category : #accessing }
RsrRegistryElement >> storageClass [
	"Return the class of the storage"

	^Array
]

{ #category : #accessing }
RsrRegistryElement >> value [

	^storage at: 1
]

{ #category : #accessing }
RsrRegistryElement >> value: anObject [

	storage
		at: 1
		put: anObject
]
