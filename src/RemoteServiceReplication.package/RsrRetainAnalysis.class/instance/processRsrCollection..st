processing
processRsrCollection: aCollection

	self
		processing: aCollection
		during: [aCollection do: [:each | self process: each]]