encoding
sendOver: aConnection

	| analysis |
	analysis := RsrRetainAnalysis
		roots: (Array with: response)
		connection: aConnection.
	analysis perform.
	retainList := analysis retainCommands.
	self encodeUsing: aConnection encoder.
	aConnection commandWriter enqueue: self