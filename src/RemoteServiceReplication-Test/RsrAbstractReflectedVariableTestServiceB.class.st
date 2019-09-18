Class {
	#name : #RsrAbstractReflectedVariableTestServiceB,
	#superclass : #RsrAbstractReflectedVariableTestServiceA,
	#instVars : [
		'varB'
	],
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrAbstractReflectedVariableTestServiceB class >> clientClass [

	^RsrReflectedVariableTestClient
]

{ #category : #accessing }
RsrAbstractReflectedVariableTestServiceB class >> serverClass [

	^RsrReflectedVariableTestServer
]
