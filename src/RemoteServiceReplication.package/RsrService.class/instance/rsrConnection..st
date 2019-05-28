private-accessing
rsrConnection: aConnection

	super rsrConnection: aConnection.
	remoteSelf := aConnection rsrForwarderClass on: self