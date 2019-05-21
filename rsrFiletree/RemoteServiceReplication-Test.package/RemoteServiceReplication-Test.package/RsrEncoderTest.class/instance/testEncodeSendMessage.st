running-immediates
testEncodeSendMessage

	| service command expectedEncoding |
	service := RsrClientNoInstVars new.
	self register: service.
	command := RsrSendMessage
		transaction: 1
		receiver: service
		selector: #return42
		arguments: #().
	command encodeUsing: RsrEncoder new.
	expectedEncoding :=
		#[0 0 0 0 0 0 0 1], "SendMessage Command"
		#[0 0 0 0 0 0 0 1], "Transaction ID"
		#[0 0 0 0 0 0 0 0], "Argument Count"
		#[0 0 0 0 0 0 0 1], "Receiver OID"
		#[0 0 0 0 0 0 0 0], "Selector Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 8], "Length of UTF-8 bytes"
		#[114 101 116 117 114 110 52 50]. "#return42"
	self
		assert: command encoding
		equals: expectedEncoding