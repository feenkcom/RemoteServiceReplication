initialization
testHappyPathForInstanceCreation
	|results|
	
	results := RsrStringEvaluationService evaluateOnServer: 'CustomerObject new'.

	"Need some better assertion here."
	self assert: results printString equals: (RsrReference new) printString.