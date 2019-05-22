running
testError

	| promise expected |
	promise := RsrPromise new.
	expected := Object new.
	[(Delay forSeconds: 1) wait.
	promise error: Error new] fork.
	self
		should: [promise value]
		raise: Error