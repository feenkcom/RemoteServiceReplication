running
testForwarding

	| rsrObject id remoteInterface forwarder message |
	rsrObject := RsrClientTestService new.
	id := self newId.
	remoteInterface := self newTestConnectionUsing: RsrForwarder.
	rsrObject
		rsrId: id;
		rsrConnection: remoteInterface.
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