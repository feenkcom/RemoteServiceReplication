"
No class-specific documentation for RsrDispatchEventLoop, hierarchy is:
Object
  RsrObject
    RsrEventLoop( process connection state)
      RsrDispatchEventLoop( queue)

"
Class {
	#name : 'RsrDispatchEventLoop',
	#superclass : 'RsrEventLoop',
	#instVars : [
		'queue'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'dispatching' }
RsrDispatchEventLoop >> dispatch: aCommand [

	queue nextPut: aCommand
]

{ #category : 'running' }
RsrDispatchEventLoop >> executeCycle [

	| item |
	item := queue next.
	item == self stopToken
		ifTrue: [^self].
	self report: item.
	item executeFor: self connection
]

{ #category : 'initialize' }
RsrDispatchEventLoop >> initialize [

	super initialize.
	queue := SharedQueue new
]

{ #category : 'accessing' }
RsrDispatchEventLoop >> priority [

	^super priority - 1
]

{ #category : 'commands' }
RsrDispatchEventLoop >> stop [

	super stop.
	queue nextPut: self stopToken
]

{ #category : 'accessing' }
RsrDispatchEventLoop >> stopToken [

	^self stoppedState
]
