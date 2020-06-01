Extension { #name : 'RsrServiceSpecies' }

{ #category : '*remoteservicereplication' }
RsrServiceSpecies class >> reflectedVariableIndicesFor: aService
do: aBlock [

	| allVariables |
	allVariables := aService class allInstVarNames.
	(self reflectedVariablesFor: aService)
		do:
			[:varName | | index |
			index := allVariables indexOf: varName.
			aBlock value: index]
]

{ #category : '*remoteservicereplication' }
RsrServiceSpecies class >> reflectedVariablesFor: aService [

	| currentClass variables |
	variables := OrderedCollection new.
	currentClass := aService class abstractClass.
	[currentClass == RsrService]
		whileFalse:
			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].
			currentClass := currentClass superclass].
	^variables
]

{ #category : '*remoteservicereplication' }
RsrServiceSpecies class >> reflectedVariablesFor: aService
do: aBlock [

	self
		reflectedVariableIndicesFor: aService
		do: [:index | aBlock value: (aService instVarAt: index)]
]
