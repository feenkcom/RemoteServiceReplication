Class {
	#name : 'RsrCommandSink',
	#superclass : 'RsrEventLoop',
	#instVars : [
		'queue'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'commands' }
RsrCommandSink >> enqueue: aCommand [

	self isActive ifTrue: [queue nextPut: aCommand]
]

{ #category : 'commands' }
RsrCommandSink >> executeCycle [

	[| command |
	command := queue next.
	command == self stopToken
		ifTrue: [^self].
	self writeCommand: command.
	(queue size = 0)
		ifTrue: [self flush]]
		on: RsrSocketClosed
		do:
			[:ex |
			self reportException: ex.
			self connection disconnected]
]

{ #category : 'commands' }
RsrCommandSink >> flush [

	self stream flush
]

{ #category : 'initialization' }
RsrCommandSink >> initialize [

	super initialize.
	queue := SharedQueue new
]

{ #category : 'commands' }
RsrCommandSink >> stop [

	super stop.
	queue nextPut: self stopToken
]

{ #category : 'accessing' }
RsrCommandSink >> stopToken [

	^self stoppedState
]

{ #category : 'writing' }
RsrCommandSink >> write: aByteArray [

	self stream nextPutAll: aByteArray
]

{ #category : 'writing' }
RsrCommandSink >> writeCommand: aCommand [

	self report: aCommand.
	aCommand writeUsing: self
]
