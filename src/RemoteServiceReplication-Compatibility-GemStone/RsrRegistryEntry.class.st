Class {
	#name : #RsrRegistryEntry,
	#superclass : #RsrObject,
	#instVars : [
		'service',
		'dispatcher'
	],
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #'instance-creation' }
RsrRegistryElement class >> with: anObject [

	^self new
        value: anObject;
        yourself
]

{ #category : #accessing }
RsrRegistryElement >> dispatcher [

	^dispatcher
]

{ #category : #accessing }
RsrRegistryElement >> dispacter: aMessageDispacter [

	dispatcher := aMessageDispacter
]

{ #category : #accessing }
RsrRegistryElement >> service [

	^service
]

{ #category : #accessing }
RsrRegistryElement >> service: anRsrService [

	service := anRsrService
]

{ #category : #accessing }
RsrRegistryElement >> value [

	^self service
]

{ #category : #accessing }
RsrRegistryElement >> value: anObject [

	self service: anObject
]
