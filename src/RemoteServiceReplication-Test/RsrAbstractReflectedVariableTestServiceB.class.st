Class {
	#name : #RsrAbstractReflectedVariableTestServiceB,
	#superclass : #RsrAbstractReflectedVariableTestServiceA,
	#instVars : [
		'varB'
	],
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrAbstractReflectedVariableTestServiceB class >> clientClassName [

	^#RsrReflectedVariableTestClient
]

{ #category : #accessing }
RsrAbstractReflectedVariableTestServiceB class >> serverClassName [

	^#RsrReflectedVariableTestServer
]
