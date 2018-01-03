convenience
command: aCommand
	|t|
	t := RsrEvaluateOnServer new.
	^t command: aCommand.