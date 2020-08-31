Extension { #name : 'RsrServiceSpecies' }

{ #category : '*remoteservicereplication' }
RsrServiceSpecies class >> reflectedVariableIndicesFor: aService
do: aBlock [

	| allVariables |
	Error signal.
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
	Error signal.
	variables := OrderedCollection new.
	currentClass := aService class templateClass.
	[currentClass == RsrService]
		whileFalse:
			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].
			currentClass := currentClass superclass].
	^variables
]

{ #category : '*remoteservicereplication' }
RsrServiceSpecies class >> reflectedVariablesFor: aService
do: aBlock [

	Error signal.
	self
		reflectedVariableIndicesFor: aService
		do: [:index | aBlock value: (aService instVarAt: index)]
]
