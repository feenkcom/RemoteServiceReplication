Class {
	#name : 'RsrReflectedVariableTestClient',
	#superclass : 'RsrAbstractReflectedVariableTestServiceB',
	#instVars : [
		'private'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrReflectedVariableTestClient >> setVarsToAndReturn: anObject [

	^remoteSelf setVarsToAndReturn: anObject
]
