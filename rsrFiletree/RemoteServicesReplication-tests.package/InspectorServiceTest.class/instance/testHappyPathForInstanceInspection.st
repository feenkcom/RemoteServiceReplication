initialization
testHappyPathForInstanceInspection
	|cot ref inspectResults |
	cot := CustomerObject new.
	ref := RsrReferenceServer getReference: cot.
	inspectResults := RsrInspectorClient inspect: ref.
	"Need some better assertion here."
	self assert: (inspectResults isKindOf: RsrInspector).