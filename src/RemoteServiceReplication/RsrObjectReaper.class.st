Class {
	#name : #RsrObjectReaper,
	#superclass : #Object,
	#instVars : [
		'weakStorage'
	],
	#category : #'RemoteServiceReplication-Utilities'
}

{ #category : #'instance creation' }
RsrObjectReaper class >> connection: aConnection [

	^self new
		connection: aConnection;
		yourself
]

{ #category : #accessing }
RsrObjectReaper >> connection [

	^weakStorage at: 1
]

{ #category : #accessing }
RsrObjectReaper >> connection: aConnection [

	weakStorage at: 1 put: aConnection
]

{ #category : #initialization }
RsrObjectReaper >> initialize [

	super initialize.
	weakStorage := WeakArray new: 1
]

{ #category : #reaping }
RsrObjectReaper >> reap: anOID [

	self connection ifNotNil: [:connection | connection releaseOid: anOID]
]
