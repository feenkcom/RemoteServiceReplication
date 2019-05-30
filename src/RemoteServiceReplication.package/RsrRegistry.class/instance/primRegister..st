initialization
primRegister: anRsrObject
	"Keep a weak reference to the provided object.
	The simple #add: may be an better interface."

	anRsrObject rsrId isNil
		ifTrue: [	anRsrObject rsrId: oidSpigot next].
	map
		at: anRsrObject rsrId
		put: anRsrObject