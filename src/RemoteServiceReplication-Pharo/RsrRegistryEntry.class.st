Class {
	#name : #RsrRegistryEntry,
	#superclass : #RsrObject,
	#instVars : [
		'storage',
		'strongStorage'
	],
	#category : #'RemoteServiceReplication-Pharo'
}

{ #category : #'instance creation' }
RsrRegistryEntry class >> service: aService [

	^self new
		service: aService;
		yourself
]

{ #category : #transitions }
RsrRegistryEntry >> becomeStrong [

	strongStorage
		at: 1
		put: self service
]

{ #category : #transitions }
RsrRegistryEntry >> becomeWeak [

	strongStorage
		at: 1
		put: nil
]

{ #category : #initializing }
RsrRegistryEntry >> initialize [

	super initialize.
	storage := WeakArray new: 1.
	strongStorage := Array new: 1
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
