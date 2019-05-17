tests
testHostname
	|nameToTest rst|
	
	rst := RsrServer new.
	nameToTest := 'localhost'.
	rst hostname: nameToTest.
	
	self assert: rst hostname equals: nameToTest .