Class {
	#name : 'RsrSocketStressTest',
	#superclass : 'RsrStressTest',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'running' }
RsrSocketStressTest >> setUp [

	super setUp.
	self initializeSocketConnections
]
