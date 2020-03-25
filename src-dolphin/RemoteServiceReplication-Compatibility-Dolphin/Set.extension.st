!Set methodsFor!
hash

	^self
		inject: #Set hash
		into: [:hash :each | hash bitXor: each hash]! !