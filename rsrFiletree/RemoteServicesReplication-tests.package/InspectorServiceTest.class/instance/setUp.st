initialization
setUp
	(RsrDaemon status)
		ifFalse: [ |rd|
			rd := RsrDaemon uniqueInstance.
			rd remoteServerName: 'localhost'.
			rd start.
			].