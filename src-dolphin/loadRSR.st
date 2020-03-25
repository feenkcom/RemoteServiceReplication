| packages |
packages := #(
	'RemoteServiceReplication-Base.pac'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrProtoObject.st'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrForwarder.st'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrObject.st'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrAbstractService.st'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrClassResolver.st'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrEnvironment.st'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrRegistry.st'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrGarbageCollector.st'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrScientist.st'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrSocket.st'
	'RemoteServiceReplication-Compatibility-Dolphin\RsrSpecies.extension.st'
	'RemoteServiceReplication-Compatibility-Dolphin\SequenceableCollection.extension.st'
	'RemoteServiceReplication-Compatibility-Dolphin\Set.extension.st'
	'RemoteServiceReplication.pac'
	'RemoteServiceReplication-Compatibility-Test.pac'
	'RemoteServiceReplication-Test.pac'
).
packages
	do:
		[:packageName | | stream |
		stream := (File open: packageNames) readWriteStream.
		(ChunkSourceFiler on: stream) fileIn.
		stream close].


[TestRunner open runAllTests] forkAt: 2
