initialization
retain: anRsrObject
	"Keep a strong reference to the provided object."

	mutex
		critical:
			[map
				at: anRsrObject rsrId
				put: anRsrObject.
			storage add: anRsrObject]