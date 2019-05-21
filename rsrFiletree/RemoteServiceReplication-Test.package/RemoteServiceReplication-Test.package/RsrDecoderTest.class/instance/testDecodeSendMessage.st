running
testDecodeSendMessage

	| service encoding command |
	service := RsrServerNoInstVars
		rsrId: 1
		rsrRemoteInterface: RsrMockRemoteInterface new.
	self decoder.
	registry addRemote: service.
	encoding :=
		#[0 0 0 0 0 0 0 1], "SendMessage Command"
		#[0 0 0 0 0 0 0 1], "Transaction ID"
		#[0 0 0 0 0 0 0 0], "Argument Count"
		#[0 0 0 0 0 0 0 1], "Receiver OID"
		#[0 0 0 0 0 0 0 0], "Selector Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 8], "Length of UTF-8 bytes"
		#[114 101 116 117 114 110 52 50]. "#return42"
	command := self decoder decodeCommand: encoding readStream.
	self
		assert: command class
		equals: RsrSendMessage.
	self
		assert: command transaction
		equals: 1.
	self
		assert: command receiver
		identicalTo: service.
	self
		assert: command selector
		identicalTo: #return42.
	self
		assert: command arguments
		equals: #()