accessing
send: anObject 
	|remoteServerName rd|
	rd := RsrDaemon uniqueInstance.
	remoteServerName := rd remoteRsrServer.
	^self send: anObject to: remoteServerName.
	