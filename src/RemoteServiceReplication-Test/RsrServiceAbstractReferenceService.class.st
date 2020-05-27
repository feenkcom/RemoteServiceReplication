Class {
	#name : 'RsrServiceAbstractReferenceService',
	#superclass : 'RsrService',
	#instVars : [
		'service'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrServiceAbstractReferenceService class >> abstractClassName [

	^#RsrServiceAbstractReferenceService
]

{ #category : 'accessing' }
RsrServiceAbstractReferenceService class >> clientClassName [

	^#RsrClientReferenceService
]

{ #category : 'accessing' }
RsrServiceAbstractReferenceService class >> serverClassName [

	^#RsrServerReferenceService
]

{ #category : 'instance creation' }
RsrServiceAbstractReferenceService class >> service: aService [

	^self new
		service: aService;
		yourself
]

{ #category : 'accessing' }
RsrServiceAbstractReferenceService >> service [
	^ service
]

{ #category : 'accessing' }
RsrServiceAbstractReferenceService >> service: anObject [
	service := anObject
]
