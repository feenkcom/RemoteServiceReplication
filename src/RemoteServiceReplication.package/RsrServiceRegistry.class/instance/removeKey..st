public-accessing
removeKey: aServiceName

	self deregisterName: aServiceName.
	remoteSelf deregisterName: aServiceName