accessing
remoteServerName: aServerName
	|aRsrServer|
	
	aRsrServer := RsrServer new.
	aRsrServer hostname: aServerName.
	remoteRsrServer := aRsrServer.