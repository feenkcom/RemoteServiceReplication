run
Object
	subclass: #RsrConcurrency
	instVarNames: #()
	classVars: #()
	classInstVars: #(#current)
	poolDictionaries: #()
	inDictionary: UserGlobals
%





set class RsrConcurrency class

classmethod:
resetCurrent	^current := self new
%



set class RsrConcurrency class

classmethod:
fork: aBlockat: aPriority	^self current		fork: aBlock		at: aPriority
%



set class RsrConcurrency class

classmethod:
current: concurrency	current := concurrency
%



set class RsrConcurrency class

classmethod:
fork: aBlock	^self current fork: aBlock
%



set class RsrConcurrency class

classmethod:
current	^current ifNil: [self resetCurrent]
%

set class RsrConcurrency

method:
fork: aBlockat: aPriority	^aBlock forkAt: aPriority
%



set class RsrConcurrency

method:
fork: aBlock	^aBlock fork
%