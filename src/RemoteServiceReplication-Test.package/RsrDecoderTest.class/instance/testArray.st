running
testArray

	| encoding actual |
	encoding :=
		#[0 0 0 0 0 0 0 9], "Object Type - Array"
		#[0 0 0 0 0 0 0 1], "OID - 1"
		#[0 0 0 0 0 0 0 5], "size"
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 6], "nil"
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 7], "true"
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 8], "false"
		#[0 0 0 0 0 0 0 1], "self oid"
		#[0 0 0 0 0 0 0 0], "Immediate OID"
		#[0 0 0 0 0 0 0 3], "Positive Integer"
		#[0 0 0 0 0 0 0 1], "num bytes in integer"
		#[15].
	actual := self decoder decodeObject: encoding readStream.
	self
		assert: actual class
		equals: RsrArray.
	self
		assert: actual rsrId
		equals: 1.
	self
		assert: (actual at: 1)
		equals: nil.
	self
		assert: (actual at: 2)
		equals: true.
	self
		assert: (actual at: 3)
		equals: false.
	self
		assert: (actual at: 4)
		equals: actual.
	self
		assert: (actual at: 5)
		equals: 15