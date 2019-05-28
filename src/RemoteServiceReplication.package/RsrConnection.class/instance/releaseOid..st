private-coordination
releaseOid: anOid

	| command |
	self isOpen
		ifFalse: [^self].
	command := RsrReleaseObjects oids: (Array with: anOid).
	command encodeUsing: self encoder.
	commandWriter enqueue: command