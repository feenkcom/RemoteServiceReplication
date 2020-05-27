Class {
	#name : 'RsrReflectedVariableTestServer',
	#superclass : 'RsrAbstractReflectedVariableTestServiceB',
	#instVars : [
		'private'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrReflectedVariableTestServer >> setVarsToAndReturn: anObject [

	^varA := varB := anObject
]
