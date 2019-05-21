running
testForwarding

	| rsrObject id remoteInterface forwarder message |
	rsrObject := RsrClientTestService new.
	id := self newId.
	remoteInterface := self newTestRemoteInterfaceUsing: RsrForwarder.
	rsrObject
		rsrId: id;
		rsrRemoteInterface: remoteInterface.
	forwarder := rsrObject remoteSelf.
	message := RsrSendMessage
		transaction: 1
		receiver: rsrObject
		selector: #arg1:arg2:
		arguments: #(15 42).
	forwarder
		arg1: 15
		arg2: 42.
	self
		assert: remoteInterface lastMessage
		equals: message