executing
executeFor: aConnection

	| promise |
	promise := aConnection promises
		at: transaction
		ifAbsent:
			[self todo: 'What should really happen if an invalid response is received?'.
			^nil].
	promise fulfill: response