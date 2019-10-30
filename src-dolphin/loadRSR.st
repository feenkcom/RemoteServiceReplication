| packages |
packages := #(
	'RemoteServiceReplication-Base'
	'RemoteServiceReplication-Compatibility-Dolphin'
	'RemoteServiceReplication-Compatibility-Test'
	'RemoteServiceReplication'
	'RemoteServiceReplication-Test'
).
packages
	do:
		[:packageName | | stream |
		stream := (File open: packageName, '.st') readWriteStream.
		(ChunkSourceFiler on: stream) fileIn.
		stream close].


[TestRunner open runAllTests] forkAt: 2