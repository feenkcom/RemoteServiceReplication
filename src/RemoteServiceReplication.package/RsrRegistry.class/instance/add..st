initialization
add: anRsrObject

	(self includes: anRsrObject)
		ifTrue: [^anRsrObject].
	map
		at: anRsrObject rsrId
		put: anRsrObject.
	storage add: anRsrObject
