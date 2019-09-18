Class {
	#name : #RsrAbstractTestService,
	#superclass : #RsrService,
	#instVars : [
		'sharedVariable'
	],
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrAbstractTestService class >> clientClass [

	^RsrClientTestService
]

{ #category : #accessing }
RsrAbstractTestService class >> serverClass [

	^RsrServerTestService
]

{ #category : #accessing }
RsrAbstractTestService >> remoteSelf [

	^remoteSelf
]

{ #category : #accessing }
RsrAbstractTestService >> sharedVariable [

	^sharedVariable
]

{ #category : #accessing }
RsrAbstractTestService >> sharedVariable: anObject [

	sharedVariable := anObject
]
