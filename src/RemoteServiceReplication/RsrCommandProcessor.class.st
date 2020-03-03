Class {
	#name : #RsrCommandProcessor,
	#superclass : #RsrObject,
	#instVars : [
		'process',
		'connection',
		'state'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrCommandProcessor class >> on: aConnection [

	^self new
		connection: aConnection;
		yourself
]

{ #category : #accessing }
RsrCommandProcessor >> connection [

	^connection
]

{ #category : #accessing }
RsrCommandProcessor >> connection: anObject [

	connection := anObject
]

{ #category : #running }
RsrCommandProcessor >> executeCycle [

	self subclassResponsibility
]

{ #category : #initialization }
RsrCommandProcessor >> initialize [

	super initialize.
	state := self stoppedState
]

{ #category : #testing }
RsrCommandProcessor >> isActive [

	^state == self runningState
]

{ #category : #running }
RsrCommandProcessor >> log [

	^RsrLogWithPrefix
		prefix: self class name asString
		log: self connection log
]

{ #category : #running }
RsrCommandProcessor >> log: aString [

	self log debug: aString
]

{ #category : #accessing }
RsrCommandProcessor >> priority [

	^Processor lowIOPriority
]

{ #category : #running }
RsrCommandProcessor >> report: aCommand [

	aCommand reportOn: self log
]

{ #category : #running }
RsrCommandProcessor >> reportException: anException [

	self log: '', self class name, '/', anException description
]

{ #category : #running }
RsrCommandProcessor >> runLoop [

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
RsrCommandProcessor >> runningState [

	^#Running
]

{ #category : #commands }
RsrCommandProcessor >> start [

	state := self runningState.
	process := RsrConcurrency
		fork: [self runLoop]
		at: self priority
]

{ #category : #commands }
RsrCommandProcessor >> stop [

	self isActive ifFalse: [^self].
	state := self stoppedState.
	self connection close.
	self stream close
]

{ #category : #accessing }
RsrCommandProcessor >> stoppedState [

	^#Stop
]

{ #category : #accessing }
RsrCommandProcessor >> stream [

	^self connection stream
]
