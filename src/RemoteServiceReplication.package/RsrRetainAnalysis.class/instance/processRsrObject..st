processing
processRsrObject: anRsrObject

	self
		processing: anRsrObject
		during:
			[self
				reflectedVariablesOf: anRsrObject
				do: [:each | self process: each]]