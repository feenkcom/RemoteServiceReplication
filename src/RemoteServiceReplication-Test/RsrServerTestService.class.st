Class {
	#name : 'RsrServerTestService',
	#superclass : 'RsrTestService',
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
