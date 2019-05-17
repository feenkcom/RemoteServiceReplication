convenience
execute
	|results|
	
	[ results :=  Compiler evaluate: stringToExecute] on: Exception do: [ 
	results := nil ].

	"TODO: Need a better method for handling errors."
	^RsrReference wrap: results.
