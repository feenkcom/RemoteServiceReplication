running
testMarkDirtyMarkClean

	| localObject |
	localObject := self newTestObject.
	self
		assert: localObject isDirty;
		deny: localObject isClean.
	localObject markClean.
	self
		deny: localObject isDirty;
		assert: localObject isClean.
	localObject markDirty.
	self
		assert: localObject isDirty;
		deny: localObject isClean