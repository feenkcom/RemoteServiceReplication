Class {
	#name : #RsrEventLoop,
	#superclass : #RsrObject,
	#instVars : [
		'process',
		'connection',
		'state'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrEventLoop class >> on: aConnection [

	^self new
		connection: aConnection;
		yourself
]

{ #category : #accessing }
RsrEventLoop >> connection [

	^connection
]

{ #category : #accessing }
RsrEventLoop >> connection: anObject [

	connection := anObject
]

{ #category : #running }
RsrEventLoop >> executeCycle [

	self subclassResponsibility
]

{ #category : #initialization }
RsrEventLoop >> initialize [

	super initialize.
	state := self stoppedState
]

{ #category : #testing }
RsrEventLoop >> isActive [

	^state == self runningState
]

{ #category : #running }
RsrEventLoop >> log [

	^RsrLogWithPrefix
		prefix: self class name asString
		log: self connection log
]

{ #category : #running }
RsrEventLoop >> log: aString [

	self log debug: aString
]

{ #category : #accessing }
RsrEventLoop >> priority [

	^Processor lowIOPriority
]

{ #category : #running }
RsrEventLoop >> report: aCommand [

	aCommand reportOn: self log
]

{ #category : #running }
RsrEventLoop >> reportException: anException [

	self log: '', self class name, '/', anException description
]

{ #category : #running }
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

{ #category : #accessing }
RsrEventLoop >> runningState [

	^#Running
]

{ #category : #commands }
RsrEventLoop >> start [

	state := self runningState.
	process := RsrConcurrency
		fork: [self runLoop]
		at: self priority
]

{ #category : #commands }
RsrEventLoop >> stop [

	self isActive ifFalse: [^self].
	state := self stoppedState.
	self connection close.
	self stream close
]

{ #category : #accessing }
RsrEventLoop >> stoppedState [

	^#Stop
]

{ #category : #accessing }
RsrEventLoop >> stream [

	^self connection stream
]
