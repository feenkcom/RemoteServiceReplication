accessing
stream

	^stream ifNil: [stream := SocketStream on: socket]