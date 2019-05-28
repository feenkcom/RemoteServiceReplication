asserting
assertObject: anObject
isMirrored: isMirrored
isDirty: isDirty
id: id
connection: remoteInterface

	self
		assert: anObject
		equals: anObject.
	self
		assert: anObject isMirrored
		equals: isMirrored.
	self
		assert: anObject isDirty
		equals: isDirty.
	self
		assert: anObject isClean
		equals: isDirty not.
	self
		assert: anObject rsrId
		equals: id.
	self
		assert: anObject rsrConnection
		equals: remoteInterface