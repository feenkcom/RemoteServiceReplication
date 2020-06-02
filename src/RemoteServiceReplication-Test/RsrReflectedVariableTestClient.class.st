Class {
	#name : 'RsrReflectedVariableTestClient',
	#superclass : 'RsrReflectedVariableTestServiceB',
	#instVars : [
		'private'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrReflectedVariableTestClient >> setVarsToAndReturn: anObject [

	^remoteSelf setVarsToAndReturn: anObject
]
