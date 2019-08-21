run
Object
	subclass: #RsrObject
	instVarNames: #()
	inDictionary: UserGlobals
    constraints: #()
%

set class RsrObject

classmethod:
new

    ^super new initialize
%

method:
initialize

    ^self
%

method:
isInteger: anObject

    ^anObject isKindOf: Integer
%

method:
isSymbol: anObject

    ^anObject isSymbol
%

method:
isString: anObject

    ^anObject isString
%

method:
isCharacter: anObject

    ^anObject isKindOf: Character
%

method:
isArray: anObject

    ^anObject isKindOf: Array
%

method:
isDictionary: anObject

    ^anObject isKindOf: Dictionary
%

method:
isByteArray: anObject

	^anObject isKindOf: ByteArray
%

method:
isSet: anObject

	^anObject isKindOf: Set
%

method:
isOrderedCollection: anObject

	^anObject isKindOf: OrderedCollection
%

method:
isDateTime: anObject

	^anObject isKindOf: DateAndTime
%