Class {
	#name : 'RsrMessageDispatcher',
	#superclass : 'RsrDispatchEventLoop',
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrMessageDispatcher class >> startOn: aConnection [

	^(self on: aConnection)
		start;
		yourself
]

{ #category : 'dispatching' }
RsrMessageDispatcher >> dispatch: aMessageSend [

	queue nextPut: aMessageSend
]

{ #category : 'running' }
RsrMessageDispatcher >> executeCycle [

	| item |
	item := queue next.
	item == self stopToken
		ifFalse: [item value]
]

{ #category : 'running' }
RsrMessageDispatcher >> stop [

	self isActive ifFalse: [^self].
	state := self stoppedState.
	self dispatch: self stopToken
]
