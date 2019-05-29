lifecycle
start

	(isOpen := socket isConnected)
		ifFalse: [^RsrConnectionClosed signal].
	stream := RsrStream on: (SocketStream on: socket).
	stream binary.
	commandReader := RsrCommandReader on: self.
	commandWriter := RsrCommandWriter on: self.
	commandReader start.
	commandWriter start.
	registry remember: serviceRegistry