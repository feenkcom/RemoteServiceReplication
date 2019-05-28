public-registration
removeKey: aServiceName

	self deregisterName: aServiceName.
	remoteSelf deregisterName: aServiceName