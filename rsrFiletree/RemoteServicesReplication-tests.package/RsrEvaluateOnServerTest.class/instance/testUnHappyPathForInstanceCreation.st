initialization
testUnHappyPathForInstanceCreation
	|cot results|
	cot := CustomerObject new.
	
	results := RsrStringEvaluationService evaluateOnServer: 'AClassThatDoesntExist new'.

	"Need some better assertion here."
	self assert: nil equals: results representedObject.