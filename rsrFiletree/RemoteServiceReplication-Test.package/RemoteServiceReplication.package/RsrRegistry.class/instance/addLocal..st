initialization
addLocal: anRsrObject
	"Add an object that originates in our local memory space.
	Retain it until notified otherwise."

	(self includes: anRsrObject)
		ifTrue: [^anRsrObject].
	anRsrObject rsrId: oidSpigot next.
	map
		at: anRsrObject rsrId
		put: anRsrObject.
	storage add: anRsrObject
