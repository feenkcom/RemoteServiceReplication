Class {
	#name : #RsrObjectReaper,
	#superclass : #Object,
	#instVars : [
		'weakStorage'
	],
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #accessing }
RsrObjectReaper class >> registry: aRegistry [

	^self new
		registry: aRegistry;
		yourself
]

{ #category : #initialization }
RsrObjectReaper >> initialize [

	super initialize.
	weakStorage := WeakArray new: 1
]

{ #category : #reaping }
RsrObjectReaper >> reap: anOID [

	self registry ifNotNil: [:registry | registry reap: anOID]
]

{ #category : #accessing }
RsrObjectReaper >> registry [

	^weakStorage at: 1
]

{ #category : #accessing }
RsrObjectReaper >> registry: aRegistry [

	weakStorage at: 1 put: aRegistry
]
