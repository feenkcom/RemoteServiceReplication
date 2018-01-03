accessing
remoteServerName: serverName
	|rs|
	rs := RsrServer new.
	rs hostname: serverName.
	remoteRsrServer := rs.