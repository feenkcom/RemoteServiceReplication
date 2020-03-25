| packages |
packages := #(
	'RemoteServiceReplication-Base'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrProtoObject'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrForwarder'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrObject'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrAbstractService'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrClassResolver'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrEnvironment'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrRegistry'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrGarbageCollector'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrScientist'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrSocket'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrSpecies.extension'
	'RemoteServiceReplication-Compatibility-Dolphin\SequenceableCollection.extension'
	'RemoteServiceReplication-Compatibility-Dolphin\Set.extension'
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
