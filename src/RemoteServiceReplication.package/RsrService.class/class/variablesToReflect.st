private-accessing
variablesToReflect

	| currentClass variables |
	variables := OrderedCollection new.
	currentClass := self superclass.
	[currentClass == RsrService]
		whileFalse:
			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].
			currentClass := currentClass superclass].
	^variables