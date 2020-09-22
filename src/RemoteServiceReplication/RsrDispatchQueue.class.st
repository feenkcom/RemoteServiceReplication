"
No class-specific documentation for RsrDisaptchQueue, hierarchy is:
Object
  RsrObject
    RsrDisaptchQueue( queue process)

"
Class {
	#name : 'RsrDispatchQueue',
	#superclass : 'RsrObject',
	#instVars : [
		'queue',
		'process',
		'isActive',
		'isRunning'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'dispatching' }
RsrDispatchQueue >> async: aBlock [
	"Evaluate the block asynchronously and do not return a result"

	queue nextPut: aBlock.
	^nil
]

{ #category : 'dispatching' }
RsrDispatchQueue >> dispatch: aBlock [

	^self async: aBlock
]

{ #category : 'initializing' }
RsrDispatchQueue >> initialize [

	super initialize.
	queue := SharedQueue new
]

{ #category : 'testing' }
RsrDispatchQueue >> isRunning [

	^isRunning
]

{ #category : 'running' }
RsrDispatchQueue >> runLoop [

	[self isRunning]
		whileTrue:
			[queue next value]
]

{ #category : 'lifecycle' }
RsrDispatchQueue >> start [
	"Start processing queued events."

	isRunning := true.
	process := RsrProcessModel fork: [self runLoop]
]

{ #category : 'lifecycle' }
RsrDispatchQueue >> stop [
	"Stop process events in the dispatch queue."

	isRunning := false.
	self dispatch: []. "Ensure another action is added to the queue to ensure shutdown if it hasn't yet happened."
	process := nil
]
