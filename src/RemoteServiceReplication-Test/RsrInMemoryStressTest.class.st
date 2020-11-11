Class {
	#name : 'RsrInMemoryStressTest',
	#superclass : 'RsrStressTest',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'initializing' }
RsrInMemoryStressTest >> initializeConnections [

	self initializeInMemoryConnections
]
