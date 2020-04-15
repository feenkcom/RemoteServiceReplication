Class {
	#name : 'RsrServerTestService',
	#superclass : 'RsrAbstractTestService',
	#instVars : [
		'privateVariable'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrServerTestService >> privateVariable [

	^privateVariable
]

{ #category : 'accessing' }
RsrServerTestService >> privateVariable: anObject [

	privateVariable := anObject
]
