lifecycle
start

	(isOpen := socket isConnected)
		ifFalse: [^RsrConnectionClosed signal].
	stream := RsrStream on: (SocketStream on: socket).
	stream binary.
	serviceRegistry registerWith: registry.
	dispatcher := RsrCommandDispatcher on: self.
	commandReader := RsrCommandReader on: self.
	commandWriter := RsrCommandWriter on: self.
	dispatcher start.
	commandReader start.
	commandWriter start