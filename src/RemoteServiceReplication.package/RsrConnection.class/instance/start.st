lifecycle
start

	stream := SocketStream on: socket.
	stream binary.
	commandReader := RsrCommandReader on: self.
	commandWriter := RsrCommandWriter on: self.
	commandReader start.
	commandWriter start.