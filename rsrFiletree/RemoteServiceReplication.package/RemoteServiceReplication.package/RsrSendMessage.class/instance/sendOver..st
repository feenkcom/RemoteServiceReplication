accessing
sendOver: aConnection

	| analysis |
	analysis := RsrChangeAnalysis
		roots: self roots
		registry: aConnection registry
		encoder: aConnection encoder.
	analysis perform.
	retainList := analysis dirtyList.
	aConnection commandWriter enqueue: self