running
testServiceNoInstVars

	| rootService encodedObject expectedEncoding |
	rootService := RsrClientNoInstVars new.
	self register: rootService.
	encodedObject := self encoder encode: rootService.
	expectedEncoding := self serviceNoInstVarsEncoding.
	self
		assert: encodedObject encoding
		equals: expectedEncoding