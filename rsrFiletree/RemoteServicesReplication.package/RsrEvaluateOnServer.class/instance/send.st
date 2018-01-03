convenience
send
	|rsr t |
	rsr := RsrDaemon uniqueInstance.
	t := rsr send: self.
	^t