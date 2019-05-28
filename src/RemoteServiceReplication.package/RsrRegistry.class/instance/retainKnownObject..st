initialization
retainKnownObject: anRsrObject
	"Keep a strong reference to the provided object."

	mutex
		critical:
			[(storage includes: anRsrObject)
				ifTrue: [^self].
			map
				at: anRsrObject rsrId
				put: anRsrObject.
			storage add: anRsrObject]