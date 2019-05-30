initialization
retain: anRsrObject
	"Keep a strong reference to the provided object."

	mutex
		critical:
			[self primRegister: anRsrObject.
			storage add: anRsrObject]