Class {
	#name : 'RsrReflectedVariableTestServer',
	#superclass : 'RsrReflectedVariableTestServiceB',
	#instVars : [
		'private'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrReflectedVariableTestServer >> setVarsToAndReturn: anObject [

	^varA := varB := anObject
]
