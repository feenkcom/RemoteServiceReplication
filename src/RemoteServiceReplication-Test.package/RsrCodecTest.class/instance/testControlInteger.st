running-immediates
testControlInteger
	
	self
		assertControlInteger: 0
		encoding: #[0 0 0 0 0 0 0 0].
	self
		assertControlInteger: 1
		encoding: #[0 0 0 0 0 0 0 1].
	self
		assertControlInteger: -1
		encoding: #[255 255 255 255 255 255 255 255].
	self
		assertControlInteger: (2 raisedTo: 63) - 1
		encoding: #[127 255 255 255 255 255 255 255].
	self
		assertControlInteger: (2 raisedTo: 63) negated
		encoding: #[128 0 0 0 0 0 0 0]