convenience
send
	|rnc|
	rnc := RsrNetworkClient new.
	^ rnc sendAndReceive: self.