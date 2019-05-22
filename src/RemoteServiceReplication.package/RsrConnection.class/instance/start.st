lifecycle
start

	| stream |
	stream := SocketStream on: socket.
	stream binary.
	commandReader := RsrCommandReader
		stream: stream
		decoder: self decoder
		connection: self.
	commandWriter := RsrCommandWriter on: stream.
	commandReader start.
	commandWriter start.