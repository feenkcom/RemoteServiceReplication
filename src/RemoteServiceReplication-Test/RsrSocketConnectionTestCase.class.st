Class {
	#name : 'RsrSocketConnectionTestCase',
	#superclass : 'RsrConnectionTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'running' }
RsrSocketConnectionTestCase >> setUp [

	super setUp.
	self initializeSocketConnections
]
