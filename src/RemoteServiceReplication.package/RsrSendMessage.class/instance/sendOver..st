accessing
sendOver: aConnection

	| analysis |
	analysis := RsrRetainAnalysis
		roots: self roots
		registry: aConnection registry
		encoder: aConnection encoder.
	analysis perform.
	retainList := analysis retainCommands.
	aConnection commandWriter enqueue: self