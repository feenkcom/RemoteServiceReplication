running
testServiceReferenceService

	| rootService referencedService encodedObject expectedEncoding |
	referencedService := self decode: self referencedServiceEncoding.
	self
		assert: referencedService class
		equals: RsrServerNoInstVars.
	self
		assert: referencedService rsrId
		equals: 2.
	rootService := self decode: self rootServiceEncoding.
	self
		assert: rootService class
		equals: RsrServerReferenceService.
	self
		assert: rootService service
		equals: referencedService