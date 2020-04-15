Class {
	#name : 'RsrAbstractReflectedVariableTestServiceB',
	#superclass : 'RsrAbstractReflectedVariableTestServiceA',
	#instVars : [
		'varB'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrAbstractReflectedVariableTestServiceB class >> clientClassName [

	^#RsrReflectedVariableTestClient
]

{ #category : 'accessing' }
RsrAbstractReflectedVariableTestServiceB class >> serverClassName [

	^#RsrReflectedVariableTestServer
]
