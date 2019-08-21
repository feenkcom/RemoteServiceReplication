Class {
	#name : #RsrReflection,
	#superclass : #Object,
	#category : #'RemoteServiceReplication-Utilities'
}

{ #category : #reflection }
RsrReflection class >> reflectedVariableIndecesOf: anRsrObject
do: aBlock [

	| allVariables |
	allVariables := anRsrObject class allInstVarNames.
	anRsrObject class variablesToReflect
		do:
			[:varName | | index |
			index := allVariables indexOf: varName.
			aBlock value: index]
]

{ #category : #reflection }
RsrReflection class >> reflectedVariableNamesOf: aService [

	^aService class variablesToReflect
]

{ #category : #reflection }
RsrReflection class >> reflectedVariablesOf: anRsrObject
do: aBlock [

	RsrReflection
		reflectedVariableIndecesOf: anRsrObject
		do:
			[:index |
			aBlock value: (anRsrObject instVarAt: index)]
]
