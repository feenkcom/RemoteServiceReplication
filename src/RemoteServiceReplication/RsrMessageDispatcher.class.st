Class {
	#name : #RsrMessageDispatcher,
	#superclass : #RsrDispatchEventLoop,
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrMessageDispatcher class >> startOn: aConnection [

	^(self on: aConnection)
		start;
		yourself
]

{ #category : #dispatching }
RsrMessageDispatcher >> dispatch: aMessageSend [

	queue nextPut: aMessageSend
]

{ #category : #running }
RsrMessageDispatcher >> executeCycle [

	| item |
	item := queue next.
	item == self stopToken
		ifFalse: [item value]
]

{ #category : #running }
RsrMessageDispatcher >> stop [

	self dispatch: self stopToken
]
