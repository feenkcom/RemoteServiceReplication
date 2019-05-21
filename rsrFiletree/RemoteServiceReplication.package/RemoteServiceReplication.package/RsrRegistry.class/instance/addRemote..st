initialization
addRemote: anRsrObject
	"Add an object that originates with out peer. Store it but do not
	retain it."

	(self includes: anRsrObject)
		ifTrue: [^anRsrObject].
	map
		at: anRsrObject rsrId
		put: anRsrObject
