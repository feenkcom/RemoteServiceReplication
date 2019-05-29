running
testRetainNewObject

	| encoding command |
	encoding :=
		#[0 0 0 0 0 0 0 0], "RetainObject Command"
		#[0 0 0 0 0 0 0 0], "ServiceType Object"
		#[0 0 0 0 0 0 0 1], "Service OID"
		#[0 0 0 0 0 0 0 0], "Inst Var Count"
		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 19], "Length of UTF-8 bytes"
		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"
	command := self decoder decodeCommand: encoding readStream.
	self
		assert: command object class
		equals: RsrServerNoInstVars.
	self
		assert: command object rsrId
		equals: 1.
	self assert: (registry includes: command object)