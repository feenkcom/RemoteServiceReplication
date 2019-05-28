private-registration
deregisterName: aServiceName

	self critical: [services removeKey: aServiceName ifAbsent: []]