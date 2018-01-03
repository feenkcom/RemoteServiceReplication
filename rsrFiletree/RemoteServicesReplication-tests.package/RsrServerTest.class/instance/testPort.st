tests
testPort
	|rst|
	
	rst := RsrServer new.
	
	self assert: rst port equals: rst defaultPort.