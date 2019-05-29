lifecycle
close
	isOpen
		ifFalse: [ ^ self ].
	isOpen := false.
	commandReader stop.
	commandWriter stop.
	self promises do: [ :each | each error: RsrConnectionClosed new ].
	self cleanup