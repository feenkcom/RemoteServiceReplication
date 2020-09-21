"
No class-specific documentation for RsrEventLoop, hierarchy is:
Object
  RsrObject
    RsrEventLoop( process connection state)

"
Class {
	#name : 'RsrEventLoop',
	#superclass : 'RsrObject',
	#instVars : [
		'process',
		'channel',
		'state'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrEventLoop class >> on: aChannel [

	^self new
		channel: aChannel;
		yourself
]

{ #category : 'accessing' }
RsrEventLoop >> channel [

	^channel
]

{ #category : 'accessing' }
RsrEventLoop >> channel: aChannel [

	channel := aChannel
]

{ #category : 'accessing' }
RsrEventLoop >> connection [

	^self channel connection
]

{ #category : 'running' }
RsrEventLoop >> executeCycle [

	self subclassResponsibility
]

{ #category : 'initialization' }
RsrEventLoop >> initialize [

	super initialize.
	state := self stoppedState
]

{ #category : 'testing' }
RsrEventLoop >> isActive [

	^state == self runningState
]

{ #category : 'testing' }
RsrEventLoop >> isProcessActive [

	^process ~~ nil
]

{ #category : 'running' }
RsrEventLoop >> log [

	^RsrLogWithPrefix
		prefix: self class name asString
		log: self connection log
]

{ #category : 'running' }
RsrEventLoop >> log: aString [

	self log debug: aString
]

{ #category : 'accessing' }
RsrEventLoop >> priority [

	^Processor lowIOPriority
]

{ #category : 'running' }
RsrEventLoop >> report: aCommand [

	aCommand reportOn: self log
]

{ #category : 'running' }
RsrEventLoop >> reportException: anException [

	self log: '', self class name, '/', anException description
]

{ #category : 'running' }
RsrEventLoop >> runLoop [

	[self isActive]
		whileTrue:
			[[self executeCycle]
				on: Error
				do:
					[:ex |
					self reportException: ex.
					self connection unknownError: ex]]
]

{ #category : 'accessing' }
RsrEventLoop >> runningState [

	^#Running
]

{ #category : 'commands' }
RsrEventLoop >> start [

	state := self runningState.
	process := RsrProcessModel
		fork: [self runLoop.
				process := nil]
		at: self priority
]

{ #category : 'commands' }
RsrEventLoop >> stop [

	self isActive ifFalse: [^self].
	state := self stoppedState
]

{ #category : 'accessing' }
RsrEventLoop >> stoppedState [

	^#Stop
]

{ #category : 'accessing' }
RsrEventLoop >> stream [

	^self channel stream
]
