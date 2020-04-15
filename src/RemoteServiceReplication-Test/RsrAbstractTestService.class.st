Class {
	#name : 'RsrAbstractTestService',
	#superclass : 'RsrService',
	#instVars : [
		'sharedVariable'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrAbstractTestService class >> clientClassName [

	^#RsrClientTestService
]

{ #category : 'accessing' }
RsrAbstractTestService class >> serverClassName [

	^#RsrServerTestService
]

{ #category : 'accessing' }
RsrAbstractTestService >> remoteSelf [

	^remoteSelf
]

{ #category : 'accessing' }
RsrAbstractTestService >> sharedVariable [

	^sharedVariable
]

{ #category : 'accessing' }
RsrAbstractTestService >> sharedVariable: anObject [

	sharedVariable := anObject
]
