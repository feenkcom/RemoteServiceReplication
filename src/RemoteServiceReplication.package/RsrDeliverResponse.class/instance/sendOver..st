encoding
sendOver: aConnection

	| analysis |
	analysis := RsrRetainAnalysis
		roots: (Array with: response)
		registry: aConnection registry
		encoder: aConnection encoder.
	analysis perform.
	retainList := analysis retainCommands.
	self encodeUsing: aConnection encoder.
	aConnection commandWriter enqueue: self