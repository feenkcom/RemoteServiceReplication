convenience
evaluateOnServer: aCommand
	stringToExecute := aCommand.
	^self send.