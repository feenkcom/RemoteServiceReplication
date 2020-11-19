Class {
	#name : 'RsrInMemoryConnectionSpecification',
	#superclass : 'RsrInternalConnectionSpecification',
	#category : 'RemoteServiceReplication'
}

{ #category : 'connecting' }
RsrInMemoryConnectionSpecification >> connect [
	"Establish an internal Connection pair via SharedQueues."

	| aQueue bQueue channelA channelB |
	aQueue := SharedQueue new.
	bQueue := SharedQueue new.
	channelA := RsrInMemoryChannel
		inQueue: aQueue
		outQueue: bQueue.
	channelB := RsrInMemoryChannel
		inQueue: bQueue
		outQueue: aQueue.
	connectionA := RsrConnection
		channel: channelA
		transactionSpigot: RsrThreadSafeNumericSpigot naturals
		oidSpigot: RsrThreadSafeNumericSpigot naturals.
	connectionB := RsrConnection
		channel: channelB
		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated
		oidSpigot: RsrThreadSafeNumericSpigot naturals negated.
	connectionA open.
	connectionB open.
	self assertOpen
]
