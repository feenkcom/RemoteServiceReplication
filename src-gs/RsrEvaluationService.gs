run
RsrService
	subclass: #RsrAbstractEvaluationService
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrAbstractEvaluationService
	subclass: #RsrEvaluationClient
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrAbstractEvaluationService
	subclass: #RsrEvaluationServer
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

set class RsrAbstractEvaluationService

classmethod:
clientClass

    ^RsrEvaluationClient
%

classmethod:
serverClass

    ^RsrEvaluationServer
%

method:
evaluate: aString

    ^self error: 'Unable to evaluate: ', aString