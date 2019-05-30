initialization
register: anRsrObject
	"Keep a weak reference to the provided object.
	The simple #add: may be an better interface."

	mutex
		critical:
			[map
				at: anRsrObject rsrId
				put: anRsrObject]