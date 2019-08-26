run
RsrObject
	subclass: #RsrEnvironment
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

classmethod:
ifPharo: aPharoBlock
ifGemStone: aGemStoneBlock

    ^aGemStoneBlock value
%