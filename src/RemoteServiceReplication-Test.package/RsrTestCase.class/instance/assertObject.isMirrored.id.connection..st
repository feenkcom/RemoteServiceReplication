asserting
assertObject: anObject
isMirrored: isMirrored
id: id
connection: remoteInterface

	self
		assert: anObject
		equals: anObject.
	self
		assert: anObject isMirrored
		equals: isMirrored.
	self
		assert: anObject rsrId
		equals: id.
	self
		assert: anObject rsrConnection
		equals: remoteInterface