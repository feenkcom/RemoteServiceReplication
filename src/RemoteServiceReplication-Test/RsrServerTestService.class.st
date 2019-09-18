Class {
	#name : #RsrServerTestService,
	#superclass : #RsrAbstractTestService,
	#instVars : [
		'privateVariable'
	],
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrServerTestService >> privateVariable [

	^privateVariable
]

{ #category : #accessing }
RsrServerTestService >> privateVariable: anObject [

	privateVariable := anObject
]
