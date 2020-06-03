Class {
	#name : 'RsrRemoteAction',
	#superclass : 'RsrService',
	#instVars : [
		'sharedVariable'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrRemoteAction class >> templateClassName [

	^#RsrRemoteAction
]

{ #category : 'accessing' }
RsrRemoteAction >> sharedVariable [

	^sharedVariable
]

{ #category : 'accessing' }
RsrRemoteAction >> sharedVariable: anObject [

	sharedVariable := anObject
]
