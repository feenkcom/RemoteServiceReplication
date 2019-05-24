accessing
stream

	^stream ifNil: [stream := RsrStream on: (SocketStream on: socket)]