Class {
	#name : #RsrCommandProcessor,
	#superclass : #RsrObject,
	#instVars : [
		'process',
		'connection',
		'isRunning'
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
	isRunning := false
]

{ #category : #running }
RsrCommandProcessor >> log [

	^self connection log
]

{ #category : #running }
RsrCommandProcessor >> log: anException [

	self log error: anException description
]

{ #category : #accessing }
RsrCommandProcessor >> priority [

	^Processor lowIOPriority
]

{ #category : #running }
RsrCommandProcessor >> runLoop [

	[isRunning]
		whileTrue:
			[[self executeCycle]
				on: Error
				do:
					[:ex |
					self log: ex.
					self connection unknownError: ex]]
]

{ #category : #commands }
RsrCommandProcessor >> start [

	isRunning := true.
	process := [self runLoop]
		forkAt: self priority
		"named: self class name asString, ' loop'"
]

{ #category : #commands }
RsrCommandProcessor >> stop [

	isRunning ifFalse: [^self].
	isRunning := false.
	self connection close.
	self stream close.
	process terminate.
	process := nil
]

{ #category : #accessing }
RsrCommandProcessor >> stream [

	^self connection stream
]
