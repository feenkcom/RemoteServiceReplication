Class {
	#name : #RsrMessageDispatcher,
	#superclass : #RsrDispatchEventLoop,
	#category : #'RemoteServiceReplication-Communications'
}

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
