running-immediates
testEncodeRetainObject

	| service command expectedEncoding |
	service := RsrClientNoInstVars new.
	self register: service.
	command := RsrRetainObject object: service.
	command encodeUsing: RsrEncoder new.
	expectedEncoding :=
		#[0 0 0 0 0 0 0 0], "RetainObject Command"
		#[0 0 0 0 0 0 0 0], "ServiceType Object"
		#[0 0 0 0 0 0 0 1], "Service OID"
		#[0 0 0 0 0 0 0 0], "Inst Var Count"
		#[0 0 0 0 0 0 0 0], "Service Name Symbol Reference"
		#[0 0 0 0 0 0 0 1], "Object Type for Symbol"
		#[0 0 0 0 0 0 0 19], "Length of UTF-8 bytes"
		#[82 115 114 83 101 114 118 101 114 78 111 73 110 115 116 86 97 114 115]. "#RsrServerNoInstVars"
	self
		assert: command encoding
		equals: expectedEncoding