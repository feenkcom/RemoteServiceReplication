accessing
sendOver: aConnection

	| analysis promise |
	analysis := RsrRetainAnalysis
		roots: self roots
		registry: aConnection registry
		encoder: aConnection encoder.
	analysis perform.
	retainList := analysis retainCommands.
	self encodeUsing: aConnection encoder.
	promise := RsrPromise new.
	aConnection promises
		at: transaction
		put: promise.
	aConnection commandWriter enqueue: self.
	^promise