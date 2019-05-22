accessing
value

	self waitForFulfillment.
	error isNil
		ifFalse: [error copy signal].
	^value