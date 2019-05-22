running
testDeliverResponse

	| response encoding command |
	response := #responseSymbol.
	encoding :=
		#[0 0 0 0 0 0 0 2], "DeliverResponse Command"
		#[0 0 0 0 0 0 0 1], "Transaction Id"
		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 14], "Length of UTF-8 bytes"
		#[114 101 115 112 111 110 115 101 83 121 109 98 111 108]. "#responseSymbol"
	command := self decoder decode: encoding readStream.
	self
		assert: command class
		equals: RsrDeliverResponse.
	self
		assert: command transaction
		equals: 1.
	self
		assert: command response
		equals: response