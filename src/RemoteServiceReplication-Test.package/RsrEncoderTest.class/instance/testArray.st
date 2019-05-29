running-immediates
testArray

	| array expected actual |
	array := RsrArray new: 5.
	array at: 1 put: nil.
	array at: 2 put: true.
	array at: 3 put: false.
	array at: 4 put: array.
	array at: 5 put: 15.
	self register: array.
	expected :=
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
	actual := self encoder encodeObject: array.
	self
		assert: actual
		equals: expected