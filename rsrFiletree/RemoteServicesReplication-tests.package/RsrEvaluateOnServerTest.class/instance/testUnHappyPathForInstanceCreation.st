initialization
testUnHappyPathForInstanceCreation
	|cot results|
	cot := CustomerObject new.
	
	results := RsrEvaluateOnServer command: 'AClassThatDoesntExist new'.

	"Need some better assertion here."
	self assert: nil equals: results representedObject.