Extension { #name : #RsrReflection }

{ #category : #'*RemoteServiceReplication-Test' }
RsrReflection class >> allInstancesOf: aClass [

	^RsrEnvironment
		ifPharo: [aClass allInstances]
		ifGemStone: [aClass instancesInMemory]
		ifDolphin: [aClass allInstances]
]
