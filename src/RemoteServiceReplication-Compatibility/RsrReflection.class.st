Class {
	#name : #RsrReflection,
	#superclass : #Object,
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #reflection }
RsrReflection class >> reflectedVariableNamesOf: aService [

	^aService class variablesToReflect
]

{ #category : #reflection }
RsrReflection class >> reflectedVariableNamesOf: anRsrObject
do: aBlock [

	anRsrObject class variablesToReflect
		do:
			[:varName |
			aBlock value: varName]
]
