Class {
	#name : #RsrServiceReaper,
	#superclass : #RsrObject,
	#instVars : [
		'weakStorage'
	],
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #'instance creation' }
RsrServiceReaper class >> for: aRegistry [

	^self new
		registry: aRegistry;
		yourself
]

{ #category : #'instance creation' }
RsrServiceReaper >> initialize [

	super initialize.
	weakStorage := WeakArray new: 1
]

{ #category : #reaping }
RsrServiceReaper >> reap: aKey [

	self registry ifNotNil: [:registry | registry reap: aKey]
]

{ #category : #accessing }
RsrServiceReaper >> registry [

	^weakStorage at: 1
]

{ #category : #accessing }
RsrServiceReaper >> registry: aRegistry [

	weakStorage
		at: 1
		put: aRegistry
]
