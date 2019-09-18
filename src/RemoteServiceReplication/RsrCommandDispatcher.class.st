Class {
	#name : #RsrCommandDispatcher,
	#superclass : #RsrCommandProcessor,
	#instVars : [
		'queue'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #dispatching }
RsrCommandDispatcher >> dispatch: aCommand [

	queue nextPut: aCommand
]

{ #category : #running }
RsrCommandDispatcher >> executeCycle [

	| item |
	item := queue next.
	item == self stopToken
		ifFalse: [item executeFor: connection]
]

{ #category : #initialize }
RsrCommandDispatcher >> initialize [

	super initialize.
	queue := SharedQueue new
]

{ #category : #accessing }
RsrCommandDispatcher >> priority [

	^super priority - 1
]

{ #category : #commands }
RsrCommandDispatcher >> stop [

	super stop.
	queue nextPut: self stopToken
]

{ #category : #accessing }
RsrCommandDispatcher >> stopToken [

	^self stoppedState
]
