Class {
	#name : 'RsrForwarderTest',
	#superclass : 'RsrTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'running' }
RsrForwarderTest >> testForwarding [

	| rsrObject id remoteInterface forwarder message |
	rsrObject := RsrClientTestService new.
	id := 1.
	remoteInterface := RsrMockConnection forwarderClass: RsrForwarder.
	rsrObject
		_id: id
		connection: remoteInterface.
	forwarder := rsrObject remoteSelf.
	forwarder
		arg1: 15
		arg2: 42.
	message := remoteInterface lastMessage.
	self
		assert: message transaction
		equals: 1.
	self
		assert: message receiver
		equals: rsrObject.
	self
		assert: message selector
		equals: #arg1:arg2:.
	self
		assert: message arguments
		equals: #(15 42).
]
