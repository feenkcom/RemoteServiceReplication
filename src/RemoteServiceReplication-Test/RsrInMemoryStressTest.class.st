Class {
	#name : 'RsrInMemoryStressTest',
	#superclass : 'RsrStressTest',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'running' }
RsrInMemoryStressTest >> setUp [

	super setUp.
	self initializeInMemoryConnections
]
