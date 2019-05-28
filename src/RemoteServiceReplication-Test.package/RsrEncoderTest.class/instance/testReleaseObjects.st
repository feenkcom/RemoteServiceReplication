running-immediates
testReleaseObjects

	| command expectedEncoding |
	command := RsrReleaseObjects oids: #(1 2 3 4 5).
	command encodeUsing: RsrEncoder new.
	expectedEncoding :=
		#[0 0 0 0 0 0 0 3], "ReleaseObjects Command"
		#[0 0 0 0 0 0 0 5], "Num OIDS"
		#[0 0 0 0 0 0 0 1], "First OID"
		#[0 0 0 0 0 0 0 2],
		#[0 0 0 0 0 0 0 3],
		#[0 0 0 0 0 0 0 4],
		#[0 0 0 0 0 0 0 5]. "Last OID"
	self
		assert: command encoding
		equals: expectedEncoding