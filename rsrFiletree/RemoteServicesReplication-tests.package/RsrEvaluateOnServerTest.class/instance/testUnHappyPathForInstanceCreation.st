initialization
testUnHappyPathForInstanceCreation
	|results|
		
	results := RsrStringEvaluationService evaluateOnServer: 'AClassThatDoesntExist new'.

	"Need some better assertion here."
	self assert: nil equals: results representedObject.