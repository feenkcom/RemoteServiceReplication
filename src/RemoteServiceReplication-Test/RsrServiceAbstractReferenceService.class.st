Class {
	#name : #RsrServiceAbstractReferenceService,
	#superclass : #RsrService,
	#instVars : [
		'service'
	],
	#category : 'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrServiceAbstractReferenceService class >> clientClass [

	^RsrClientReferenceService
]

{ #category : #accessing }
RsrServiceAbstractReferenceService class >> serverClass [

	^RsrServerReferenceService
]

{ #category : #'instance creation' }
RsrServiceAbstractReferenceService class >> service: aService [

	^self new
		service: aService;
		yourself
]

{ #category : #accessing }
RsrServiceAbstractReferenceService >> service [
	^ service
]

{ #category : #accessing }
RsrServiceAbstractReferenceService >> service: anObject [
	service := anObject
]
