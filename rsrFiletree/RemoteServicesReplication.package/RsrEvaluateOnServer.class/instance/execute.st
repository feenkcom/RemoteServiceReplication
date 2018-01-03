convenience
execute
	|results|
	results := Compiler evaluate: stringToExecute.
	^RsrReference wrap: results.