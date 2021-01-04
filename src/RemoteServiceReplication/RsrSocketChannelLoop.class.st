"
No class-specific documentation for RsrEventLoop, hierarchy is:
Object
  RsrObject
    RsrEventLoop( process connection state)

"
Class {
	#name : 'RsrSocketChannelLoop',
	#superclass : 'RsrObject',
	#instVars : [
		'process',
		'channel',
		'state'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrSocketChannelLoop class >> on: aChannel [

	^self new
		channel: aChannel;
		yourself
]

{ #category : 'accessing' }
RsrSocketChannelLoop >> channel [

	^channel
]

{ #category : 'accessing' }
RsrSocketChannelLoop >> channel: aChannel [

	channel := aChannel
]

{ #category : 'running' }
RsrSocketChannelLoop >> executeCycle [

	self subclassResponsibility
]

{ #category : 'initialization' }
RsrSocketChannelLoop >> initialize [

	super initialize.
	state := self stoppedState
]

{ #category : 'testing' }
RsrSocketChannelLoop >> isActive [

	^state == self runningState
]

{ #category : 'testing' }
RsrSocketChannelLoop >> isProcessActive [

	^process ~~ nil
]

{ #category : 'running' }
RsrSocketChannelLoop >> log [

	^RsrLogWithPrefix
		prefix: self class name asString
		log: self channel log
]

{ #category : 'running' }
RsrSocketChannelLoop >> log: aString [

	self log debug: aString
]

{ #category : 'accessing' }
RsrSocketChannelLoop >> priority [

	^Processor lowIOPriority
]

{ #category : 'running' }
RsrSocketChannelLoop >> report: aCommand [

	aCommand reportOn: self log
]

{ #category : 'running' }
RsrSocketChannelLoop >> reportException: anException [

	self log: anException description
]

{ #category : 'running' }
RsrSocketChannelLoop >> runLoop [

	[self isActive]
		whileTrue:
			[[self executeCycle]
				on: Error
				do:
					[:ex |
					self reportException: ex.
					self channel genericError: ex]]
]

{ #category : 'accessing' }
RsrSocketChannelLoop >> runLoopName [
	"Return the name of the associated run loop.
	This name is assigned to the Process used to execute the run loop."

	^self subclassResponsibility
]

{ #category : 'accessing' }
RsrSocketChannelLoop >> runningState [

	^#Running
]

{ #category : 'commands' }
RsrSocketChannelLoop >> start [

	state := self runningState.
	process := RsrProcessModel
		fork: [self runLoop.
				process := nil]
		at: self priority
		named: self runLoopName
]

{ #category : 'commands' }
RsrSocketChannelLoop >> stop [

	self isActive ifFalse: [^self].
	state := self stoppedState
]

{ #category : 'accessing' }
RsrSocketChannelLoop >> stoppedState [

	^#Stop
]

{ #category : 'accessing' }
RsrSocketChannelLoop >> stream [

	^self channel stream
]
