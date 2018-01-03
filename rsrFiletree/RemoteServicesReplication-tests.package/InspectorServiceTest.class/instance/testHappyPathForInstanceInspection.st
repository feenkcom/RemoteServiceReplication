initialization
testHappyPathForInstanceInspection
	|cot results|
	cot := CustomerObject new.
	
	results := RsrEvaluateOnServer command: 'CustomerObject new'.

	"Need some better assertion here."
	self assert: results  equals: 'CustomerObject'.