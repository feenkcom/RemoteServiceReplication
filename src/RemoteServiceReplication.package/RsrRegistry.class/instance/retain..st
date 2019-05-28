initialization
retain: anRsrObject
	"Keep a strong reference to the provided object."

	mutex
		critical:
			[(storage includes: anRsrObject)
				ifTrue: [^self].
			anRsrObject rsrId: oidSpigot next.
			map
				at: anRsrObject rsrId
				put: anRsrObject.
			storage add: anRsrObject]