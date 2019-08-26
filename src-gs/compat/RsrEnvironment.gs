run
RsrObject
	subclass: #RsrEnvironment
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

set class RsrEnvironment

classmethod:
ifPharo: aPharoBlock
ifGemStone: aGemStoneBlock

    ^aGemStoneBlock value
%