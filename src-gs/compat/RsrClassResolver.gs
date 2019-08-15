run
Object
	subclass: #RsrClassResolver
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

set class RsrClassResolver

classmethod:
classNamed: aClassName

	^self
		classNamed: aClassName
		ifAbsent: [self error: 'Unknown class']
%

classmethod:
classNamed: aClassName
ifAbsent: aBlock

	| assoc |
	assoc := System myUserProfile resolveSymbol: aClassName.
	^assoc
		ifNil: aBlock
		ifNotNil: [assoc value]
%