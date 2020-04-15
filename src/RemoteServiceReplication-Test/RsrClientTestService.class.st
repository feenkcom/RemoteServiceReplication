Class {
	#name : 'RsrClientTestService',
	#superclass : 'RsrAbstractTestService',
	#instVars : [
		'privateVariable'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrClientTestService >> privateVariable [

	^privateVariable
]

{ #category : 'accessing' }
RsrClientTestService >> privateVariable: anObject [

	privateVariable := anObject
]
