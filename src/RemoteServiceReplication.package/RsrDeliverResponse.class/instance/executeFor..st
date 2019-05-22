executing
executeFor: aConnection

	| promise |
	promise := aConnection promises
		removeKey: transaction
		ifAbsent:
			[^self error: 'Handle unknown transaction'].
	promise fulfill: response