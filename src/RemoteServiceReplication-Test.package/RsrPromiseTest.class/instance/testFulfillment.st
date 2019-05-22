running
testFulfillment

	| promise expected |
	promise := RsrPromise new.
	expected := Object new.
	[(Delay forSeconds: 1) wait.
	promise fulfill: expected] fork.
	self
		assert: promise value
		identicalTo: expected