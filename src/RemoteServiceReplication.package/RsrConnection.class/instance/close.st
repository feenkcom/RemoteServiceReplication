lifecycle
close

	self isOpen
		ifFalse: [^self].
	commandReader stop.
	commandWriter stop.
	self promises do: [:each | each error: (RsrConnectionClosed new)]