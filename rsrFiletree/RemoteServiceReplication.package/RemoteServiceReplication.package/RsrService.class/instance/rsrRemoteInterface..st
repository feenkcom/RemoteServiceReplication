private-accessing
rsrRemoteInterface: anRsrInterface.

	super rsrRemoteInterface: anRsrInterface.
	remoteSelf := anRsrInterface rsrForwarderClass rsrObject: self